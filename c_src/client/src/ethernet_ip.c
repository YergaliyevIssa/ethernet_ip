/*----------------------------------------------------------------
* Copyright (c) 2022 Faceplate
*
* This file is provided to you under the Apache License,
* Version 2.0 (the "License"); you may not use this file
* except in compliance with the License.  You may obtain
* a copy of the License at
*
*   http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
----------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <base64.h>
#include "ethernet_ip.h"

// cJSON* on_ok(cJSON* response);
// cJSON* on_error(char* text);
cJSON* ethernet_ip_read(cJSON* request, char **error);
cJSON* ethernet_ip_write(cJSON* request, char **error);
cJSON* ethernet_ip_create_tags(cJSON *request, char **error);
cJSON* ethernet_ip_destroy_tags(cJSON *request, char **error);
cJSON* ethernet_ip_browse_tags(cJSON *request, char **error);
cJSON* on_request(char *method, cJSON *args, char **error);


cJSON* read_from_tag(int32_t tag_id, char *tag_type);
int write_to_tag(int32_t tag_id, char *tag_type, cJSON* Value);

void check_status(int32_t* tags, int size); 
char* decode_error(int status_code);

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


cJSON* on_request(char *method, cJSON *args, char **error){

    cJSON *response = NULL;
    // Handle the request
    LOGTRACE("handle the request %s", method);

    if (strcmp(method, "create") == 0){
        response = ethernet_ip_create_tags( args, error );
    }else if( strcmp(method, "destroy") == 0){
        response = ethernet_ip_destroy_tags( args, error );
    }else if (strcmp(method, "read") == 0){
        response = ethernet_ip_read( args, error );
    }else if (strcmp(method, "write") == 0){
        response = ethernet_ip_write( args, error );
    }else if (strcmp(method, "browse_tags") == 0){
        response = ethernet_ip_browse_tags( args, error );
    } else{
        *error = "invalid method";
    }

    return response;
}


cJSON* ethernet_ip_create_tags(cJSON* args, char **error) {
    cJSON* tag_string_json = cJSON_GetObjectItemCaseSensitive(args, "tag_string");
    char* tag_string_pattern = tag_string_json -> valuestring;
    cJSON* tag_names = cJSON_GetObjectItemCaseSensitive(args, "tag_names");

    int size = cJSON_GetArraySize( tag_names );
    int32_t* tags = (int32_t*)malloc(sizeof(int32_t) * size);

    int index = 0;

    cJSON* tag_name_json = NULL;
    cJSON_ArrayForEach(tag_name_json, tag_names) {
        char tag_string[200] = ""; 
        strcat(tag_string, tag_string_pattern);
        strcat(tag_string, tag_name_json -> valuestring);
        tags[index] = plc_tag_create(tag_string, 0);
        index++;
    }
    check_status(tags, size);
    
    for (index = 0; index < size; index++) {
        LOGTRACE("tags[%d] = %d", index, tags[index]);
    }

    cJSON* result = cJSON_CreateIntArray(tags, size);
    free(tags);

    return result;
}


cJSON* ethernet_ip_destroy_tags(cJSON* args, char **error) {
    if ( !cJSON_IsArray(args) ) {
        *error = "Array must be passed";
        return NULL;
    }
    cJSON* tag_id = NULL;
    cJSON_ArrayForEach(tag_id, args) {
        plc_tag_destroy((int32_t)(tag_id -> valuedouble));
    }
    // int status = plc_tag_destroy((int32_t)(request -> valuedouble));
    // if (status < 0) {
    //     return on_error("Cannot destroy tag");
    // }
    return cJSON_CreateString("ok");
}

cJSON* ethernet_ip_read(cJSON* args, char **error) {
    if ( !cJSON_IsArray(args) ) {
        *error = "Array must be passed";
        return NULL;
    } 
    cJSON *tag_info = NULL;
    int size = cJSON_GetArraySize(args);

    int32_t *tags = (int32_t*)malloc(sizeof(int32_t) * size);
    int index = 0;

    cJSON_ArrayForEach(tag_info, args) {
        cJSON* TagId = cJSON_GetObjectItemCaseSensitive(tag_info, "tag_id"); 
        tags[index] = (int32_t)(TagId -> valuedouble);
        plc_tag_read(tags[index], 0);
        index += 1;
    }
    check_status(tags, size);

    for (index = 0; index < size; index++) {
        LOGTRACE("tags[%d] = %d", index, tags[index]);
    }

    tag_info = NULL;
    index = 0;

    cJSON* response = cJSON_CreateArray();

    cJSON_ArrayForEach(tag_info, args) {
        cJSON *Type = cJSON_GetObjectItemCaseSensitive(tag_info, "type");
        cJSON *read_result = read_from_tag(tags[index], Type -> valuestring);
        cJSON_AddItemToArray(response, read_result);
        index += 1;
    }

    free(tags);
    return response;
}

cJSON* ethernet_ip_write(cJSON* args, char **error) {
    if ( !cJSON_IsArray(args) ) {
        *error = "Array must be passed";
        return NULL;
    } 

    int size = cJSON_GetArraySize(args);
    int32_t *tags = (int32_t*)malloc(sizeof(int32_t) * size);
    int* write_status = (int*)malloc(sizeof(int) * size);
    int index = 0;

    cJSON *tag_info = NULL;
    cJSON_ArrayForEach(tag_info, args) {
        cJSON* TagId = cJSON_GetObjectItemCaseSensitive(tag_info, "tag_id");
        cJSON* Value = cJSON_GetObjectItemCaseSensitive(tag_info, "value");
        cJSON* Type = cJSON_GetObjectItemCaseSensitive(tag_info, "type");
        tags[index] = (int32_t)(TagId -> valuedouble);
        write_status[index] = write_to_tag(tags[index], Type -> valuestring, Value);
        plc_tag_write(tags[index], 0);
        index += 1;
    }

    check_status(tags, size);

    cJSON* response = cJSON_CreateArray();
    for (index = 0; index < size; index++) {
        cJSON* write_result = NULL;
        if (tags[index] < 0) {
            write_status[index] = tags[index];
        }
        if (write_status[index] < 0) {
            write_result = cJSON_CreateString(decode_error(write_status[index]));
        } else {
            write_result = cJSON_CreateString("ok");
        }
        cJSON_AddItemToArray(response, write_result);
    }

    free(tags);
    free(write_status);
    return response;
}

cJSON* ethernet_ip_browse_tags(cJSON* request, char **error) {
    //tag_entry_s* tag_list = NULL;
    char* tag_string_base = request -> valuestring;
    int status = 0;
    cJSON* tag_info = browse_tags(tag_string_base, &status);
    if (status == 0) {
        return tag_info;
    } else {
        *error = tag_info -> valuestring;
        cJSON_Delete(tag_info);
        return NULL;
    }
    
}


//------------------Internal helpers--------------------------------------------
void check_status(int32_t* tags, int size) {
    int done = 0, i = 0;
    int status; 
    while (!done) {
        done = 1;
        for (i = 0; i < size; i++) {
            status = plc_tag_status(tags[i]);
            if (status == PLCTAG_STATUS_PENDING) {
                done = 0;
            } else if (status < 0) {
                tags[i] = status;
            }
        }
        if (!done) {
            // sleep one ms
            usleep(1000);
        }
    }
    return;
}

cJSON* read_from_tag(int32_t tag_id, char *tag_type) {
    cJSON *result = cJSON_CreateObject();
    if (tag_id < 0) {
        cJSON_AddStringToObject(result, "error", decode_error(tag_id));
        return result;
    }
    if (strcmp(tag_type, "SINT") == 0) {
        int8_t value = plc_tag_get_int8(tag_id, 0);
        cJSON_AddNumberToObject(result, "value", value);
    } else if (strcmp(tag_type, "USINT") == 0) {
        uint8_t value = plc_tag_get_uint8(tag_id, 0);
        cJSON_AddNumberToObject(result, "value", value);
    } else if (strcmp(tag_type, "INT") == 0) {
        int16_t value = plc_tag_get_int16(tag_id, 0);
        cJSON_AddNumberToObject(result, "value", value);
    } else if (strcmp(tag_type, "UINT") == 0) {
        uint16_t value = plc_tag_get_uint16(tag_id, 0);
        cJSON_AddNumberToObject(result, "value", value);
    } else if (strcmp(tag_type, "DINT") == 0) {
        int32_t value = plc_tag_get_int32(tag_id, 0);
        cJSON_AddNumberToObject(result, "value", value);
    } else if (strcmp(tag_type, "UDINT") == 0) {
        uint32_t value = plc_tag_get_uint32(tag_id, 0);
        cJSON_AddNumberToObject(result, "value", value);
    } else if (strcmp(tag_type, "LINT") == 0) {
        int64_t value = plc_tag_get_int64(tag_id, 0);
        cJSON_AddNumberToObject(result, "value", value);
    } else if (strcmp(tag_type, "ULINT") == 0) {
        uint64_t value = plc_tag_get_uint64(tag_id, 0);
        cJSON_AddNumberToObject(result, "value", value);
    } else if (strcmp(tag_type, "REAL") == 0) {
        float value = plc_tag_get_float32(tag_id, 0);
        cJSON_AddNumberToObject(result, "value", value);
    } else if (strcmp(tag_type, "LREAL") == 0) {
        double value = plc_tag_get_float64(tag_id, 0);
        cJSON_AddNumberToObject(result, "value", value);
    } else {
        cJSON_AddStringToObject(result, "error", "type not supported");
    }

    return result;

}

int write_to_tag(int32_t tag_id, char *tag_type, cJSON* Value) {
    int result = 0;
    if (tag_id < 0) {
        return tag_id;
    }
    if (strcmp(tag_type, "SINT") == 0) {
        int8_t val = (int8_t)Value -> valuedouble;
        result = plc_tag_set_int8(tag_id, 0, val);
    } else if (strcmp(tag_type, "USINT") == 0) {
        uint8_t val = (uint8_t)Value -> valuedouble;
        result = plc_tag_set_uint8(tag_id, 0, val);
        
    } else if (strcmp(tag_type, "INT") == 0) {
        int16_t val = (int16_t)Value -> valuedouble;
        result = plc_tag_set_int16(tag_id, 0, val);
       
    } else if (strcmp(tag_type, "UINT") == 0) {
        uint16_t val = (uint16_t)Value -> valuedouble;
        result = plc_tag_set_uint16(tag_id, 0, val);
       
    } else if (strcmp(tag_type, "DINT") == 0) {
        int32_t val = (int32_t)Value -> valuedouble;
        result = plc_tag_set_int32(tag_id, 0, val);
        
    } else if (strcmp(tag_type, "UDINT") == 0) {
        uint32_t val = (uint32_t)Value -> valuedouble;
        result = plc_tag_set_uint32(tag_id, 0, val);
        
    } else if (strcmp(tag_type, "LINT") == 0) {
        int64_t val = (int64_t)Value -> valuedouble;
        result = plc_tag_set_int64(tag_id, 0, val);
        
    } else if (strcmp(tag_type, "ULINT") == 0) {
        uint64_t val = (uint64_t)Value -> valuedouble;
        result = plc_tag_set_uint64(tag_id, 0, val);
        
    } else if (strcmp(tag_type, "REAL") == 0) {
        float val = (float)Value -> valuedouble;
        result = plc_tag_set_float32(tag_id, 0, val);
        
    } else if (strcmp(tag_type, "LREAL") == 0) {
        double val  = (double)Value -> valuedouble;
        result = plc_tag_set_float64(tag_id, 0, val);
        
    } else {
        // magic ()
       result = -100;
    }

    return result;

}

char* decode_error(int status_code) {
    if (status_code == -100) {
        return "Type is not supported";
    }
    return plc_tag_decode_error(status_code);
}


int main(int argc, char* argv[]) {

    eport_loop(&on_request);
    return 0;
}