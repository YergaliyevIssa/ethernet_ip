#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <eport.h>
#include "ethernet_ip.h"

cJSON* on_ok(cJSON* response);
cJSON* on_error(char* text);
cJSON* ethernet_ip_read(cJSON* request);
cJSON* ethernet_ip_write(cJSON* request);
cJSON* ethernet_ip_create_tag(cJSON* request);
cJSON* ethernet_ip_destroy_tag(cJSON* request);
cJSON* ethernet_ip_browse_tags(cJSON* request);
char* on_request( char *requestString );

// Source discovery helpers
typedef struct tag_entry_s {
    struct tag_entry_s *next;
    char *name;
    struct tag_entry_s *parent;
} tag_entry_s;

int open_tag(char *base, char *tag_name);
int get_tag_list(int32_t tag,  tag_entry_s **tag_list,  tag_entry_s *parent);
int process_tag_entry(int32_t tag, int *offset, uint16_t *last_tag_id,  tag_entry_s **tag_list,  tag_entry_s *parent);
void free_tag_list(tag_entry_s **tag_list);



char* on_request( char *requestString ){
    cJSON *response;
    char *responseString;
    ETHERNET_IP_CLIENT_REQUEST request = {};

    // Parse the request
    LOGDEBUG("DEBUG: parsing the request\r\n");
    if (parse_request( requestString, &request ) != 0){
        response = on_error("invalid request");
    }else{
        // Handle the request
        LOGDEBUG("DEBUG: handle the request\r\n");
        if( request.cmd == READ_DATA ){
            response = ethernet_ip_read( request.body );
        } else if (request.cmd == WRITE_DATA ){
            response = ethernet_ip_write( request.body );
        }else if (request.cmd == CREATE_TAG ){
            response = ethernet_ip_create_tag( request.body );
        }else if (request.cmd == DESTROY_TAG ){
            response = ethernet_ip_destroy_tag( request.body );
        } else if (request.cmd == BROWSE_TAGS) {
            response = ethernet_ip_browse_tags( request.body );
        } else{
            response = on_error("unsupported command type");
        }
    }

    // Reply (purges the response)
    responseString = create_response( &request, response );

    purge_request( &request );
    LOGDEBUG("DEBUG: response %s\r\n",responseString);

    return responseString;
}

cJSON* on_ok(cJSON *result){
    cJSON *response = cJSON_CreateObject();
    if ( cJSON_AddStringToObject(response, "type", "ok") == NULL) {
        goto error;
    }
    if ( !cJSON_AddItemToObject(response, "result", result) ) {
        goto error;
    }
    return response;

error:
    cJSON_Delete( response );
    return NULL;
}

cJSON* on_error(char* text){
    cJSON *response = cJSON_CreateObject();
    if (cJSON_AddStringToObject(response, "type", "error") == NULL) {
        goto error;
    }
    if (cJSON_AddStringToObject(response, "text", text) == NULL) {
        goto error;
    }
    return response;

error:
    cJSON_Delete( response );
    return NULL;
}


cJSON* ethernet_ip_create_tag(cJSON* request) {
    char* connection_path = request -> valuestring;
    int32_t tag_id = plc_tag_create(connection_path, TIMEOUT);
    if (tag_id < 0) {
        return on_error("Cannot create tag");
    }
    return on_ok(cJSON_CreateNumber(tag_id));
}


cJSON* ethernet_ip_destroy_tag(cJSON* request) {
    plc_tag_destroy((int32_t)(request -> valuedouble));
    return on_ok(NULL);
}

cJSON* ethernet_ip_read(cJSON* request) {
    cJSON* TagId = cJSON_GetObjectItemCaseSensitive(request, "tag_id");
    cJSON* Offset = cJSON_GetObjectItemCaseSensitive(request, "offset");
    cJSON* Length = cJSON_GetObjectItemCaseSensitive(request, "length");
    cJSON* response = NULL;
    int32_t tag_id = (int32_t)(TagId -> valuedouble);
    int offset = (int)(Offset -> valuedouble);
    int length = (int)(Length -> valuedouble);
    int rc = plc_tag_read(tag_id, TIMEOUT);
    if (rc != PLCTAG_STATUS_OK) {
        return on_error("Read error");
    }
    uint8_t* buffer[10000];
    int status = plc_tag_get_raw_bytes(tag_id, offset, buffer, length);
    if (status < 0) {
        response = on_error("Cannot read data form tag");
        return response;
    }
    buffer[length] = '\0';
    response = cJSON_CreateString(buffer);
    return on_ok(response);
}

cJSON* ethernet_ip_write(cJSON* request) {
    cJSON* TagId = cJSON_GetObjectItemCaseSensitive(request, "tag_id");
    cJSON* Offset = cJSON_GetObjectItemCaseSensitive(request, "offset");
    cJSON* Length = cJSON_GetObjectItemCaseSensitive(request, "length");
    cJSON* Value = cJSON_GetObjectItemCaseSensitive(request, "value");
    cJSON* response = NULL;

    int32_t tag_id = (int32_t)(TagId -> valuedouble);
    int offset = (int)(Offset -> valuedouble);
    int length = (int)(Length -> valuedouble);
    uint8_t* data = (uint8_t*)(Value -> valuestring);
    
    int status = plc_tag_set_raw_bytes(tag_id, offset, data, length);
    if (status < 0) {
        response = on_error("Cannot write data to tag");
        return response;
    }
    response = cJSON_CreateString("ok");
    return on_ok(response);
}


cJSON* ethernet_ip_browse_tags(cJSON* request) {
    tag_entry_s* tag_list = NULL;
    char* connection_path_base = request -> valuestring;
    int32_t controller_listing_tag = 0;
    int32_t program_listing_tag = 0;


    controller_listing_tag = open_tag(connection_path_base, "@tags");
    if (controller_listing_tag <= 0) {
        return on_error("Cannot browse tags");
    }
    int rc = get_tag_list(controller_listing_tag, &tag_list, NULL);
    if(rc != PLCTAG_STATUS_OK) {
        free_tag_list(&tag_list);
        return on_error("Unable to get tag list or no tags visible in the target PLC");
    }

    /*
     * now loop through the tags and get the list for the program tags.
     *
     * This is safe because we push the new tags on the front of the list and
     * so do not change any existing tag in the list.
     */
    for(tag_entry_s *entry = tag_list; entry; entry = entry->next) {
        if(strncmp(entry->name, "Program:", strlen("Program:")) == 0) {
            char buf[256] = {0};

            /* this is a program tag, check for its tags. */
            snprintf(buf, sizeof(buf), "%s.@tags", entry->name);

            program_listing_tag = open_tag(connection_path_base, buf);
            if(program_listing_tag <= 0) {
                //free_tag_list(&tag_list);
                //fprintf(stderr, "Unable to create listing tag, error %s!\n", plc_tag_decode_error(program_listing_tag));
            }

            rc = get_tag_list(program_listing_tag, &tag_list, entry);
            if(rc != PLCTAG_STATUS_OK) {
                //fprintf(stderr, "Unable to get program tag list or no tags visible in the target PLC, error %s!\n", plc_tag_decode_error(rc));
            }

            plc_tag_destroy(program_listing_tag);
        }
    }

    char tag_names[10000][256];
    int tag_num = 0;

    /* output all the tags. */
    for(tag_entry_s *tag = tag_list; tag; tag = tag->next) {
        char buf[256] = {0};
        if(!tag->parent) {
            sprintf(buf, "%s", tag->name);
        } else {
            sprintf(buf, "%s.%s", tag->parent->name, tag->name);
        }
        strncpy(tag_names[tag_num++], buf, 256);
    }

    /* clean up memory */
    free_tag_list(&tag_list);

    /* Destroy this at the end to keep the session open. */
    plc_tag_destroy(controller_listing_tag);


    cJSON* array_names = cJSON_CreateArray();
    for (int i = 0;i < tag_num; i++) {
       cJSON* array_item = cJSON_CreateString(tag_names[i]);
       array_names = cJSON_AddItemToArray(array_names, array_item); 
    }

    return on_ok(array_names);
}

//////////////////////////////////////
// Internal helpers for browse tags // 
//////////////////////////////////////
int get_tag_list(int32_t tag,  tag_entry_s **tag_list, tag_entry_s *parent)
{
    int rc = PLCTAG_STATUS_OK;
    uint16_t last_tag_entry_id = 0;
    int payload_size = 0;
    int offset = 0;

    /* go get it. */
    rc = plc_tag_read(tag, TIMEOUT);
    if(rc != PLCTAG_STATUS_OK) {
        fprintf(stderr, "Error %s trying to send CIP request!\n", plc_tag_decode_error(rc));
    }

    /* process the raw data. */
    payload_size = plc_tag_get_size(tag);
    if(payload_size < 0) {
        fprintf(stderr, "Error getting payload size!\n");
    }

    /* check the payload size. */
    if(payload_size < 4) {
        fprintf(stderr, "Unexpectedly small payload size %d!\n", payload_size);
    }

    /* process each entry */
    do {
        rc = process_tag_entry(tag, &offset, &last_tag_entry_id, tag_list, parent);
    } while(rc == PLCTAG_STATUS_OK && offset < payload_size);

    return PLCTAG_STATUS_OK;
}

int process_tag_entry(int32_t tag, int *offset, uint16_t *last_tag_id,  tag_entry_s **tag_list,  tag_entry_s *parent)
{
    int rc = PLCTAG_STATUS_OK;
    int tag_name_len = 0;
    char *tag_name = NULL;
    tag_entry_s *tag_entry = NULL;

    /* each entry looks like this:
        uint32_t instance_id    monotonically increasing but not contiguous
        uint16_t symbol_type    type of the symbol.
        uint16_t element_length length of one array element in bytes.
        uint32_t array_dims[3]  array dimensions.
        uint16_t string_len     string length count.
        uint8_t string_data[]   string bytes (string_len of them)
    */

    *last_tag_id = (uint16_t)plc_tag_get_uint32(tag, *offset);
    *offset += (4 + 2 + 2 + 4 + 4 + 4);

    /* get the tag name length. */
    tag_name_len = plc_tag_get_string_length(tag, *offset);
    // *offset += 2;

    /* allocate space for the the tag name.  Add one for the zero termination. */
    tag_name = calloc((size_t)(unsigned int)(tag_name_len + 1), 1);
    if(!tag_name) {
        fprintf(stderr, "Unable to allocate memory for the tag name!\n");
        return PLCTAG_ERR_NO_MEM;
    }

    rc = plc_tag_get_string(tag, *offset, tag_name, tag_name_len + 1);
    if(rc != PLCTAG_STATUS_OK) {
        fprintf(stderr, "Unable to get tag name string, error %s!\n", plc_tag_decode_error(rc));
        free(tag_name);
        return rc;
    }

    /* skip past the string. */
    (*offset) += plc_tag_get_string_total_length(tag, *offset);

    /* allocate the new tag entry. */
    tag_entry = calloc(1, sizeof(*tag_entry));

    if(!tag_entry) {
        return PLCTAG_ERR_NO_MEM;
    }
    /* fill in the fields. */
    tag_entry->name = tag_name;
    tag_entry->parent = parent;

    /* link it up to the list */
    tag_entry->next = *tag_list;
    *tag_list = tag_entry;

    return PLCTAG_STATUS_OK;
}


int open_tag(char *base, char *tag_name)
{
    int32_t tag = PLCTAG_ERR_CREATE;
    char tag_string[201] = {0,};

    /* build the tag string. */

    strncpy(tag_string, base, 201);

    strncat(tag_string, "&name=", 201);

    strncat(tag_string, tag_name, 201);


    tag = plc_tag_create(tag_string, TIMEOUT);
    if(tag < 0) {
        fprintf(stderr, "Unable to open tag!  Return code %s\n", plc_tag_decode_error(tag));
    }

    return tag;
}

void free_tag_list(tag_entry_s **tag_list) {
    tag_entry_s *current_tag = *tag_list;
    while(current_tag) {
        tag_entry_s *tag = current_tag;

        /* unlink */
        current_tag = current_tag->next;

        if(tag->name) {
            free(tag->name);
            tag->name = NULL;
        }

        free(tag);
    }
    *tag_list = NULL;
    return;

 }
int main(int argc, char* argv[]) {

    eport_loop(&on_request);
    return 0;
}