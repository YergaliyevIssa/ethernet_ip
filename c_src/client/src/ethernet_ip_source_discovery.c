/***************************************************************************
 *   Copyright (C) 2021 by Kyle Hayes                                      *
 *   Author Kyle Hayes  kyle.hayes@gmail.com                               *
 *                                                                         *
 * This software is available under either the Mozilla Public License      *
 * version 2.0 or the GNU LGPL version 2 (or later) license, whichever     *
 * you choose.                                                             *
 *                                                                         *
 * MPL 2.0:                                                                *
 *                                                                         *
 *   This Source Code Form is subject to the terms of the Mozilla Public   *
 *   License, v. 2.0. If a copy of the MPL was not distributed with this   *
 *   file, You can obtain one at http://mozilla.org/MPL/2.0/.              *
 *                                                                         *
 *                                                                         *
 * LGPL 2:                                                                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Library General Public License as       *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU Library General Public     *
 *   License along with this program; if not, write to the                 *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/


#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ethernet_ip.h"


#define REQUIRED_VERSION 2,4,0

#define TAG_STRING_SIZE (200)
#define TIMEOUT_MS 5000


#define TYPE_IS_STRUCT ((uint16_t)0x8000)
#define TYPE_IS_SYSTEM ((uint16_t)0x1000)
#define TYPE_DIM_MASK ((uint16_t)0x6000)
#define TYPE_UDT_ID_MASK ((uint16_t)0x0FFF)
#define TAG_DIM_MASK ((uint16_t)0x6000)


#define MAX_UDTS (1 << 12)



struct program_entry_s {
    struct program_entry_s *next;
    char *program_name;
};

struct tag_entry_s {
    struct tag_entry_s *next;
    char *name;
    struct tag_entry_s *parent;
    uint16_t type;
    uint16_t elem_size;
    uint16_t elem_count;
    uint16_t num_dimensions;
    uint16_t dimensions[3];
};


struct udt_field_entry_s {
    char *name;
    uint16_t type;
    uint16_t metadata;
    uint32_t size;
    uint32_t offset;
};


struct udt_entry_s {
    char *name;
    uint16_t id;
    uint16_t num_fields;
    uint16_t struct_handle;
    uint32_t instance_size;
    struct udt_field_entry_s fields[];
};




static void usage(void);
static char *setup_tag_string(int argc, char **argv);
static int open_tag(char *base, char *tag_name);
static int get_tag_list(int32_t tag_id, struct tag_entry_s **tag_list, struct tag_entry_s *parent);
static char* get_element_type(uint16_t element_type);
static int process_tag_entry(int32_t tag, int *offset, uint16_t *last_tag_id, struct tag_entry_s **tag_list, struct tag_entry_s *parent);
static int get_udt_definition(char *base, uint16_t udt_id);


/* a local cache of all found UDT definitions. */
static struct udt_entry_s *udts[MAX_UDTS] = { NULL };
static uint16_t udts_to_process[MAX_UDTS] = {0};
static int last_udt = 0;
static int current_udt = 0;


static int debug_level = PLCTAG_DEBUG_NONE;


cJSON* browse_tags(const char* tag_string_base)
{
    int rc = PLCTAG_STATUS_OK;
    char *host = NULL;
    char *path = NULL;
    int32_t controller_listing_tag = 0;
    int32_t program_listing_tag = 0;
    struct tag_entry_s *tag_list = NULL;
    int version_major = plc_tag_get_int_attribute(0, "version_major", 0);
    int version_minor = plc_tag_get_int_attribute(0, "version_minor", 0);
    int version_patch = plc_tag_get_int_attribute(0, "version_patch", 0);

    /* check the library version. */
    if(plc_tag_check_lib_version(REQUIRED_VERSION) != PLCTAG_STATUS_OK) {
        fprintf(stdout,"Required compatible library version %d.%d.%d not available, found %d.%d.%d!\n", REQUIRED_VERSION, version_major, version_minor, version_patch);
        return 1;
    }

    plc_tag_set_debug_level(PLCTAG_DEBUG_ERROR);

    fprintf(stdout,"Starting with library version %d.%d.%d.\n", version_major, version_minor, version_patch);

    /* clear the UDTs. */
    for(int index = 0; index < MAX_UDTS; index++) {
        udts[index] = NULL;
    }

    debug_level = plc_tag_get_int_attribute(0, "debug", PLCTAG_DEBUG_NONE);


    /* set up the tag for the listing first. */
    controller_listing_tag = open_tag(tag_string_base, "@tags");
    if(controller_listing_tag <= 0) {
        fprintf(stdout,"Unable to create listing tag, error %s!\n", plc_tag_decode_error(controller_listing_tag));
        usage();
    }

    /* get the list of controller tags. */
    rc = get_tag_list(controller_listing_tag, &tag_list, NULL);
    if(rc != PLCTAG_STATUS_OK) {
        fprintf(stdout, "Unable to get tag list or no tags visible in the target PLC, error %s!\n", plc_tag_decode_error(rc));
        usage();
    }

    if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout, "Controller tag listing tag ID: %d\n", controller_listing_tag);

    /*
     * now loop through the tags and get the list for the program tags.
     *
     * This is safe because we push the new tags on the front of the list and
     * so do not change any existing tag in the list.
     */
    for(struct tag_entry_s *entry = tag_list; entry; entry = entry->next) {
        if(strncmp(entry->name, "Program:", strlen("Program:")) == 0) {
            char buf[256] = {0};

            /* this is a program tag, check for its tags. */
            if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout, "Getting tags for program \"%s\".\n", entry->name);

            snprintf(buf, sizeof(buf), "%s.@tags", entry->name);

            program_listing_tag = open_tag(tag_string_base, buf);
            if(program_listing_tag <= 0) {
                fprintf(stdout,"Unable to create listing tag, error %s!\n", plc_tag_decode_error(program_listing_tag));
                usage();
            }

            if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout, "Program tag listing tag ID: %d\n", program_listing_tag);

            rc = get_tag_list(program_listing_tag, &tag_list, entry);
            if(rc != PLCTAG_STATUS_OK) {
                fprintf(stdout, "Unable to get program tag list or no tags visible in the target PLC, error %s!\n", plc_tag_decode_error(rc));
                usage();
            }

            plc_tag_destroy(program_listing_tag);
            if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout, "Destroying program tag listing tag ID: %d\n", program_listing_tag);
        }
    }

    /* loop through the tags and get the UDT information. */
    for(struct tag_entry_s *entry = tag_list; entry; entry = entry->next) {
        /* check the type of the tag's element type. */
        uint16_t element_type = entry->type;

        /* if this is a structure, make sure we have the definition. */
        if((element_type & TYPE_IS_STRUCT) && !(element_type & TYPE_IS_SYSTEM)) {
            uint16_t udt_id = element_type & TYPE_UDT_ID_MASK;

            udts_to_process[last_udt] = udt_id;
            last_udt++;

            if(last_udt >= MAX_UDTS) {
                plc_tag_destroy(controller_listing_tag);
                fprintf(stdout, "More than %d UDTs are requested!\n", MAX_UDTS);
                usage();
            }
        }
    }

    /* get all the UDTs that we have touched. Note that this can add UDTs to the stack to process! */
    while(current_udt < last_udt) {
        uint16_t udt_id = udts_to_process[current_udt];

        /* see if we already have it. */
        if(udts[udt_id] == NULL) {
            rc = get_udt_definition(tag_string_base, udt_id);
            if(rc != PLCTAG_STATUS_OK) {
                plc_tag_destroy(controller_listing_tag);
                fprintf(stdout, "Unable to get UDT template ID %u, error %s!\n", (unsigned int)(udt_id), plc_tag_decode_error(rc));
                usage();
            }
        } else {
            if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "Already have UDT (%04x) %s.\n", (unsigned int)udt_id, udts[udt_id]->name);
        }

        current_udt++;
    }

    cJSON* source_info = cJSON_CreateObject();
    cJSON* tag_info = NULL;
    cJSON_AddItemToObject(source_info, "tag_info", tag_info=cJSON_CreateObject());

    /* output all the tags. */
    for(struct tag_entry_s *tag = tag_list; tag; tag = tag->next) {
        char name[101];
        if(!tag->parent) {
            sprintf(name,"%s", tag->name);
        } else {
            sprintf(name, "%s.%s", tag->parent->name, tag->name);
        }
        cJSON* single_tag_info = NULL;
        cJSON_AddItemToObject(tag_info, name, single_tag_info=cJSON_CreateObject());
        cJSON_AddNumberToObject(single_tag_info, "elem_size", tag -> elem_size);
        cJSON_AddNumberToObject(single_tag_info, "elem_count", tag -> elem_count);
        char* type = get_element_type(tag->type);
        cJSON_AddStringToObject(single_tag_info, "type", type);
        // switch(tag->num_dimensions) {
        //     case 1:
        //         fprintf(stdout,"[%d]", tag->dimensions[0]);
        //         break;

        //     case 2:
        //         fprintf(stdout,"[%d,%d]", tag->dimensions[0], tag->dimensions[1]);
        //         break;

        //     case 3:
        //         fprintf(stdout,"[%d,%d,%d]", tag->dimensions[0], tag->dimensions[1], tag->dimensions[2]);
        //         break;

        //     default:
        //         break;
        // }

        /* handle the type. */
        //fprintf(stdout,"\" ");
        
        //fprintf(stdout,".  ");

        /* print the tag string */
        // if(!tag->parent) {
        //     fprintf(stdout,"tag string = \"protocol=ab-eip&gateway=%s&path=%s&plc=ControlLogix&elem_size=%u&elem_count=%u&name=%s\"\n", host, path, tag->elem_size, tag->elem_count, tag->name);
        // } else {
        //     fprintf(stdout,"tag string = \"protocol=ab-eip&gateway=%s&path=%s&plc=ControlLogix&elem_size=%u&elem_count=%u&name=%s.%s\"\n", host, path, tag->elem_size, tag->elem_count, tag->parent->name, tag->name);
        // }
    }

    fprintf(stdout,"UDTs:\n");
    cJSON* type_info = NULL; 
    cJSON_AddItemToObject(source_info, "type_info", type_info=cJSON_CreateObject());

    /* print out all the UDTs */
    for(int index=0; index < MAX_UDTS; index++) {
        struct udt_entry_s *udt = udts[index];
        cJSON* single_type_info = NULL; 
        if(udt) {
            if(udt->name) {
                cJSON_AddItemToObject(type_info, udt->name, single_type_info=cJSON_CreateObject());
            } else {
              //  fprintf(stdout," UDT *UNNAMED* (ID %x, %d bytes, struct handle %x):\n", (unsigned int)(udt->id), (int)(unsigned int)udt->instance_size, (int)(unsigned int)udt->struct_handle);
            }
            for(int field_index = 0; field_index < udt->num_fields; field_index++) {
                cJSON* single_field_info = NULL;

                
                if(udt->fields[field_index].name) {
                    cJSON_AddItemToObject(single_type_info, udt->fields[field_index].name, single_field_info=cJSON_CreateObject());
                    fprintf(stdout,"    Field %d: %s, offset %d", field_index, udt->fields[field_index].name, udt->fields[field_index].offset);
                }
                char offset[10];
                /* is it a bit? */
                if(udt->fields[field_index].type == 0xC1) {
                    sprintf(offset, "%d,%d", udt->fields[field_index].offset, (int)(unsigned int)(udt->fields[field_index].metadata));
                    /* bit type, the metadata is the bit number. */
                    fprintf(stdout,":%d", (int)(unsigned int)(udt->fields[field_index].metadata));
                } else  {
                    sprintf(offset, "%d", udt->fields[field_index].offset);
                }

                /* is it an array? */
                if(udt->fields[field_index].type & 0x2000) { /* MAGIC */
                    fprintf(stdout,", array [%d] of type ", (int)(unsigned int)(udt->fields[field_index].metadata));
                } else {
                    fprintf(stdout,", type ");
                }

                char* type = get_element_type(udt->fields[field_index].type);
                cJSON_AddStringToObject(single_field_info, "type", type);
                cJSON_AddStringToObject(single_field_info, "offset", offset);

                fprintf(stdout,".\n");
            }
        }
    }

    /* clean up memory */
    while(tag_list) {
        struct tag_entry_s *tag = tag_list;

        /* unlink */
        tag_list = tag_list->next;

        if(tag->name) {
            free(tag->name);
            tag->name = NULL;
        }

        free(tag);
    }

    for(int index=0; index < MAX_UDTS; index++) {
        struct udt_entry_s *udt = udts[index];

        if(udt) {
            if(udt->name) {
                free(udt->name);
            }

            for(int field_index=0; field_index < udt->num_fields; field_index++) {
                struct udt_field_entry_s *field = &(udt->fields[field_index]);

                if(field->name) {
                    free(field->name);
                }
            }

            free(udt);

            udts[index] = NULL;
        }
    }

    //free(tag_string_base);

    /* Destroy this at the end to keep the session open. */
    plc_tag_destroy(controller_listing_tag);

    fprintf(stdout,"SUCCESS!\n");

    return source_info;
}


void usage()
{
    fprintf(stdout, "Usage: list_tags <PLC IP> <PLC path>\nExample: list_tags 10.1.2.3 1,0\n");
    exit(1);
}


int open_tag(char *base, char *tag_name)
{
    int32_t tag = PLCTAG_ERR_CREATE;
    char tag_string[TAG_STRING_SIZE+1] = {0,};

    /* build the tag string. */
    strncpy(tag_string, base, TAG_STRING_SIZE);

    strncat(tag_string, tag_name, TAG_STRING_SIZE);

    if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "Using tag string \"%s\".\n", tag_string);

    tag = plc_tag_create(tag_string, TIMEOUT_MS);
    if(tag < 0) {
        fprintf(stdout, "Unable to open tag!  Return code %s\n", plc_tag_decode_error(tag));
        usage();
    }

    return tag;
}


int get_tag_list(int32_t tag, struct tag_entry_s **tag_list, struct tag_entry_s *parent)
{
    int rc = PLCTAG_STATUS_OK;
    uint16_t last_tag_entry_id = 0;
    int payload_size = 0;
    int offset = 0;

    /* go get it. */
    rc = plc_tag_read(tag, TIMEOUT_MS);
    if(rc != PLCTAG_STATUS_OK) {
        fprintf(stdout, "Error %s trying to send CIP request!\n", plc_tag_decode_error(rc));
        usage();
    }

    /* process the raw data. */
    payload_size = plc_tag_get_size(tag);
    if(payload_size < 0) {
        fprintf(stdout, "Error getting payload size!\n");
        usage();
    }

    if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "Listing tag read, result of size %d.\n", payload_size);

    /* check the payload size. */
    if(payload_size < 4) {
        fprintf(stdout, "Unexpectedly small payload size %d!\n", payload_size);
        usage();
    }

    /* process each entry */
    do {
        rc = process_tag_entry(tag, &offset, &last_tag_entry_id, tag_list, parent);
    } while(rc == PLCTAG_STATUS_OK && offset < payload_size);

    return PLCTAG_STATUS_OK;
}




int process_tag_entry(int32_t tag, int *offset, uint16_t *last_tag_id, struct tag_entry_s **tag_list, struct tag_entry_s *parent)
{
    int rc = PLCTAG_STATUS_OK;
    uint16_t tag_type = 0;
    uint16_t element_length = 0;
    uint16_t array_dims[3] = {0,};
    int tag_name_len = 0;
    char *tag_name = NULL;
    struct tag_entry_s *tag_entry = NULL;

    /* each entry looks like this:
        uint32_t instance_id    monotonically increasing but not contiguous
        uint16_t symbol_type    type of the symbol.
        uint16_t element_length length of one array element in bytes.
        uint32_t array_dims[3]  array dimensions.
        uint16_t string_len     string length count.
        uint8_t string_data[]   string bytes (string_len of them)
    */

    *last_tag_id = (uint16_t)plc_tag_get_uint32(tag, *offset);
    *offset += 4;

    tag_type = plc_tag_get_uint16(tag, *offset);
    *offset += 2;

    element_length = plc_tag_get_uint16(tag, *offset);
    *offset += 2;

    array_dims[0] = (uint16_t)plc_tag_get_uint32(tag, *offset);
    *offset += 4;
    array_dims[1] = (uint16_t)plc_tag_get_uint32(tag, *offset);
    *offset += 4;
    array_dims[2] = (uint16_t)plc_tag_get_uint32(tag, *offset);
    *offset += 4;

    /* get the tag name length. */
    tag_name_len = plc_tag_get_string_length(tag, *offset);
    // *offset += 2;

    /* allocate space for the the tag name.  Add one for the zero termination. */
    tag_name = calloc((size_t)(unsigned int)(tag_name_len + 1), 1);
    if(!tag_name) {
        fprintf(stdout, "Unable to allocate memory for the tag name!\n");
        return PLCTAG_ERR_NO_MEM;
    }

    rc = plc_tag_get_string(tag, *offset, tag_name, tag_name_len + 1);
    if(rc != PLCTAG_STATUS_OK) {
        fprintf(stdout, "Unable to get tag name string, error %s!\n", plc_tag_decode_error(rc));
        free(tag_name);
        return rc;
    }

    /* skip past the string. */
    (*offset) += plc_tag_get_string_total_length(tag, *offset);

    if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "Tag %s, string length: %d.\n", tag_name, (int)(unsigned int)strlen(tag_name));

    /* allocate the new tag entry. */
    tag_entry = calloc(1, sizeof(*tag_entry));

    if(!tag_entry) {
        if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "Unable to allocate memory for tag entry!\n");
        return PLCTAG_ERR_NO_MEM;
    }

    if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "Found tag name=%s, tag instance ID=%x, tag type=%x, element length (in bytes) = %d, array dimensions = (%d, %d, %d)\n", tag_name, *last_tag_id, tag_type, (int)element_length, (int)array_dims[0], (int)array_dims[1], (int)array_dims[2]);

    /* fill in the fields. */
    tag_entry->name = tag_name;
    tag_entry->parent = parent;
    tag_entry->type = tag_type;
    tag_entry->elem_size = element_length;
    tag_entry->num_dimensions = (uint16_t)((tag_type & TAG_DIM_MASK) >> 13);
    tag_entry->dimensions[0] = array_dims[0];
    tag_entry->dimensions[1] = array_dims[1];
    tag_entry->dimensions[2] = array_dims[2];

    /* calculate the element count. */
    tag_entry->elem_count = 1;
    for(uint16_t i=0; i < tag_entry->num_dimensions; i++) {
        tag_entry->elem_count = (uint16_t)((uint16_t)tag_entry->elem_count * (uint16_t)(tag_entry->dimensions[i]));
    }

    /* link it up to the list */
    tag_entry->next = *tag_list;
    *tag_list = tag_entry;

    return PLCTAG_STATUS_OK;
}



char* get_element_type(uint16_t element_type)
{
    if(element_type & TYPE_IS_SYSTEM) {
        return "system";
    } else if(element_type & TYPE_IS_STRUCT) {
        char name[50];
        strcpy(name, udts[(size_t)(unsigned int)(element_type & TYPE_UDT_ID_MASK)]->name);
        return name;
        //sprintf(stdout,"element type UDT (0x%04x) %s", (unsigned int)(element_type), udts[(size_t)(unsigned int)(element_type & TYPE_UDT_ID_MASK)]->name);
    } else {
        uint16_t atomic_type = element_type & 0xFF; /* MAGIC */
        const char *type = NULL;

        switch(atomic_type) {
            case 0xC1: type = "BOOL"; break;
            case 0xC2: type = "SINT"; break; //Signed 8-bit integer value
            case 0xC3: type = "INT"; break; //Signed 16-bit integer value
            case 0xC4: type = "DINT"; break; //Signed 32-bit integer value
            case 0xC5: type = "LINT"; break; //Signed 64-bit integer value
            case 0xC6: type = "USINT"; break; //Unsigned 8-bit integer value
            case 0xC7: type = "UINT"; break; // Unsigned 16-bit integer value
            case 0xC8: type = "UDINT"; break; // Unsigned 32-bit integer value
            case 0xC9: type = "ULINT"; break; // Unsigned 64-bit integer value
            case 0xCA: type = "REAL"; break; //  32-bit floating point value, IEEE format
            case 0xCB: type = "LREAL"; break; // 64-bit floating point value, IEEE format
        }

        if(type) {
            return type;
        } else {
           return "unknown type";
        }
    }
}


int encode_request_prefix(const char *name, uint8_t *buffer, int *encoded_size)
{
    int symbol_length_index = 0;
    /* start with the symbolic type identifier */
    *encoded_size = 0;
    buffer[*encoded_size] = (uint8_t)(unsigned int)0x91; (*encoded_size)++;

    /* dummy value for the encoded length */
    symbol_length_index = *encoded_size;
    buffer[*encoded_size] = (uint8_t)(unsigned int)0; (*encoded_size)++;

    /* copy the string. */
    for(int index=0; index < (int)(unsigned int)strlen(name); index++) {
        buffer[*encoded_size] = (uint8_t)name[index];
        (*encoded_size)++;
    }

    /* backfill the encoded size */
    buffer[symbol_length_index] = (uint8_t)(unsigned int)(strlen(name));

    /* make sure we have an even number of bytes. */
    if((*encoded_size) & 0x01) {
        buffer[*encoded_size] = 0;
        (*encoded_size)++;
    }

    return PLCTAG_STATUS_OK;
}


int get_udt_definition(char *tag_string_base, uint16_t udt_id)
{
    int rc = PLCTAG_STATUS_OK;
    int32_t udt_info_tag = 0;
    int tag_size = 0;
    char buf[32] = {0};
    int offset = 0;
    uint16_t template_id = 0;
    uint16_t num_members = 0;
    uint16_t struct_handle = 0;
    uint32_t udt_instance_size = 0;
    //uint32_t member_desc_size = 0;
    int name_len = 0;
    char *name_str = NULL;
    int name_index = 0;
    int field_index = 0;

    /* memoize, check to see if we have this type already. */
    if(udts[udt_id]) {
        return PLCTAG_STATUS_OK;
    }

    snprintf(buf, sizeof(buf), "@udt/%u", (unsigned int)udt_id);

    udt_info_tag = open_tag(tag_string_base, buf);
    if(udt_info_tag < 0) {
        fprintf(stdout, "Unable to open UDT info tag, error %s!\n", plc_tag_decode_error(udt_info_tag));
        usage();
    }

    if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout, "UDT info tag ID: %d\n", udt_info_tag);

    rc = plc_tag_read(udt_info_tag, TIMEOUT_MS);
    if(rc != PLCTAG_STATUS_OK) {
        fprintf(stdout, "Error %s while trying to read UDT info!\n", plc_tag_decode_error(rc));
        usage();
    }

    tag_size = plc_tag_get_size(udt_info_tag);

    /* the format in the tag buffer is:
     *
     * A new header of 14 bytes:
     *
     * Bytes   Meaning
     * 0-1     16-bit UDT ID
     * 2-5     32-bit UDT member description size, in 32-bit words.
     * 6-9     32-bit UDT instance size, in bytes.
     * 10-11   16-bit UDT number of members (fields).
     * 12-13   16-bit UDT handle/type.
     *
     * Then the raw field info.
     *
     * N x field info entries
     *     uint16_t field_metadata - array element count or bit field number
     *     uint16_t field_type
     *     uint32_t field_offset
     *
     * int8_t string - zero-terminated string, UDT name, but name stops at first semicolon!
     *
     * N x field names
     *     int8_t string - zero-terminated.
     *
     */

    /* get the ID, number of members and the instance size. */
    template_id = plc_tag_get_uint16(udt_info_tag, 0);
    //member_desc_size = plc_tag_get_uint32(udt_info_tag, 2);
    udt_instance_size = plc_tag_get_uint32(udt_info_tag, 6);
    num_members = plc_tag_get_uint16(udt_info_tag, 10);
    struct_handle = plc_tag_get_uint16(udt_info_tag, 12);

    /* skip past this header. */
    offset = 14;

    /* just a sanity check */
    if(template_id != udt_id) {
        fprintf(stdout, "The ID, %x, of the UDT we are reading is not the same as the UDT ID we requested, %x!\n",(unsigned int)template_id, (unsigned int)udt_id);
        usage();
    }

    /* allocate a UDT struct with this info. */
    udts[(size_t)udt_id] = calloc(1, sizeof(struct udt_entry_s) + (sizeof(struct udt_field_entry_s) * num_members));
    if(!udts[(size_t)udt_id]) {
        fprintf(stdout, "Unable to allocate a new UDT definition structure!\n");
        usage();
    }

    udts[(size_t)udt_id]->id = udt_id;
    udts[(size_t)udt_id]->num_fields = num_members;
    udts[(size_t)udt_id]->struct_handle = struct_handle;
    udts[(size_t)udt_id]->instance_size = udt_instance_size;

    /* first section is the field type and size info for all fields. */
    for(int field_index=0; field_index < udts[udt_id]->num_fields; field_index++) {
        uint16_t field_metadata = 0;
        uint16_t field_element_type = 0;
        uint32_t field_offset = 0;

        field_metadata = plc_tag_get_uint16(udt_info_tag, offset);
        offset += 2;

        field_element_type = plc_tag_get_uint16(udt_info_tag, offset);
        offset += 2;

        field_offset = plc_tag_get_uint32(udt_info_tag, offset);
        offset += 4;

        udts[udt_id]->fields[field_index].metadata = field_metadata;
        udts[udt_id]->fields[field_index].type = field_element_type;
        udts[udt_id]->fields[field_index].offset = field_offset;

        /* make sure that we have or will get any UDT field types */
        if((field_element_type & TYPE_IS_STRUCT) && !(field_element_type & TYPE_IS_SYSTEM)) {
            uint16_t child_udt = (field_element_type & TYPE_UDT_ID_MASK);

            if(!udts[child_udt]) {
                udts_to_process[last_udt] = child_udt;
                last_udt++;
            }
        }
    }

    if(debug_level >= PLCTAG_DEBUG_DETAIL) {
        fprintf(stdout, "Offset after reading field descriptors: %d.\n", offset);
    }

    /*
     * then get the template/UDT name.   This is weird.
     * Scan until we see a 0x3B, semicolon, byte.   That is the end of the
     * template name.   Actually we should look for ";n" but the semicolon
     * seems to be enough for now.
     */

    /* first get the zero-terminated string length */
    name_len = plc_tag_get_string_length(udt_info_tag, offset);
    if(name_len <=0 || name_len >= 256) {
        fprintf(stdout, "Unexpected raw UDT name length: %d!\n", name_len);
        //usage();
    }

    /* create a string for this. */
    name_str = calloc((size_t)(name_len + 1), (size_t)1);
    if(!name_str) {
        fprintf(stdout, "Unable to allocate UDT name string!\n");
        usage();
    }

    /* copy the name */
    rc = plc_tag_get_string(udt_info_tag, offset, name_str, name_len + 1);
    if(rc != PLCTAG_STATUS_OK) {
        fprintf(stdout, "Error %s retrieving UDT name string from the tag!\n", plc_tag_decode_error(rc));
        free(name_str);
        usage();
    }

    /* zero terminate the name when we hit the first semicolon. */
    for(name_index = 0; name_index < name_len && name_str[name_index] != ';'; name_index++) { };

    if(name_str[name_index] == ';') {
        name_str[name_index] = 0;
    }

    /* check the name length again. */
    name_len = (int)(unsigned int)strlen(name_str);
    if(name_len ==0 || name_len >= 256) {
        fprintf(stdout, "Unexpected UDT name length: %d!\n", name_len);
    }

    udts[udt_id]->name = name_str;

    if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "Getting data from UDT \"%s\".\n", udts[udt_id]->name);

    /* skip past the UDT name. */
    offset += plc_tag_get_string_total_length(udt_info_tag, offset);

    /*
     * This is the second section of the data, the field names.   They appear
     * to be zero terminated.
     */

    if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "Getting %d field names for UDT %s.\n", udts[udt_id]->num_fields, udts[udt_id]->name);
    if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "offset=%u, tag_size=%u.\n", offset, tag_size);

    /* loop over all fields and get name strings.  They are zero terminated. */
    for(field_index=0; field_index < udts[udt_id]->num_fields && offset < tag_size; field_index++) {
        if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "Getting name for field %u.\n", field_index);

        /* first get the zero-terminated string length */
        name_len = plc_tag_get_string_length(udt_info_tag, offset);
        if(name_len <0 || name_len >= 256) {
            plc_tag_destroy(udt_info_tag);
            fprintf(stdout, "Unexpected UDT field name length: %d!\n", name_len);
            usage();
        }

        if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "The name for field %u is %u characters long.\n", field_index, name_len);

        /* create a string for this. */
        if(name_len > 0) {
            name_str = calloc((size_t)(name_len + 1), (size_t)1);
            if(!name_str) {
                plc_tag_destroy(udt_info_tag);
                fprintf(stdout, "Unable to allocate UDT field name string!\n");
                usage();
            }

            if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "The string for field %u is at %p.\n", field_index, (void *)name_str);

            /* copy the name */
            rc = plc_tag_get_string(udt_info_tag, offset, name_str, name_len + 1);
            if(rc != PLCTAG_STATUS_OK) {
                plc_tag_destroy(udt_info_tag);
                fprintf(stdout, "Error %s retrieving UDT field name string from the tag!\n", plc_tag_decode_error(rc));
                free(name_str);
                usage();
            }

            udts[udt_id]->fields[field_index].name = name_str;

            if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "UDT field %d is \"%s\".\n", field_index, udts[udt_id]->fields[field_index].name);

            offset += plc_tag_get_string_total_length(udt_info_tag, offset);
        } else {
            /* field name was zero length. */
            udts[udt_id]->fields[field_index].name = NULL;

            if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "UDT field %d is not named.\n", field_index);

            /*
             * The string is either zero length in which case we need to bump past the null
             * terminator or it is at the end of the tag and we need to step past the edge.
             */
            offset++;
        }
    }

    /* sanity check */
    if(offset != tag_size) {
        if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "Processed %d bytes out of %d bytes.\n", offset, tag_size);
    }

    /* if we had a system tag, we might not have the full set of member/field names.  Fill in the gaps. */
    for(; field_index < udts[udt_id]->num_fields; field_index++) {
            /* field name was zero length. */
            udts[udt_id]->fields[field_index].name = NULL;

            if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout,  "UDT field %d is not named.\n", field_index);
    }

    if(debug_level >= PLCTAG_DEBUG_INFO) fprintf(stdout, "Destroying UDT info tag: %d\n", udt_info_tag);
    plc_tag_destroy(udt_info_tag);

    return PLCTAG_STATUS_OK;
}


