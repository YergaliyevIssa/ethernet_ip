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
void check_status(int32_t* tags, int size); 


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

// cJSON* on_ok(cJSON *result){
//     cJSON *response = cJSON_CreateObject();
//     if ( cJSON_AddStringToObject(response, "type", "ok") == NULL) {
//         goto error;
//     }
//     if ( !cJSON_AddItemToObject(response, "result", result) ) {
//         goto error;
//     }
//     return response;

// error:
//     cJSON_Delete( response );
//     return NULL;
// }

// cJSON* on_error(char* text){
//     cJSON *response = cJSON_CreateObject();
//     if (cJSON_AddStringToObject(response, "type", "error") == NULL) {
//         goto error;
//     }
//     if (cJSON_AddStringToObject(response, "text", text) == NULL) {
//         goto error;
//     }
//     return response;

// error:
//     cJSON_Delete( response );
//     return NULL;
// }


cJSON* ethernet_ip_create_tags(cJSON* args, char **error) {
    cJSON* tag_string_json = cJSON_GetObjectItemCaseSensitive(args, "tag_string");
    char* tag_string_pattern = tag_string_json -> valuestring;
    cJSON* tag_names = cJSON_GetObjectItemCaseSensitive(args, "tag_names");

    int size = cJSON_GetArraySize( tag_names );
    int32_t* tags = (int32_t*)malloc(sizeof(int32_t) * size);

    int i = 0;

    cJSON* tag_name_json = NULL;
    cJSON_ArrayForEach(tag_name_json, tag_names) {
        char tag_string[200]; 
        strcat(tag_string, tag_string_pattern);
        strcat(tag_string, tag_name_json -> valuestring);
        tags[i] = plc_tag_create(tag_string, 0);
        i++;
    }
    check_status(tags, size);
    
    cJSON* result = cJSON_CreateIntArray(tags, size);
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
          
        int32_t tag_id = (int32_t)(TagId -> valuedouble);
        tags[index] = plc_tag_read(tag_id, 0);
        index += 1;
    }
    check_status(tags, size);

    char** result = (char**)malloc(sizeof(char*) * size);
    tag_info = NULL;
    index = 0;
    cJSON_ArrayForEach(tag_info, args) {
        cJSON* TagId = cJSON_GetObjectItemCaseSensitive(tag_info, "tag_id");
        cJSON* Offset = cJSON_GetObjectItemCaseSensitive(tag_info, "offset");
        cJSON* Length = cJSON_GetObjectItemCaseSensitive(tag_info, "length"); 
        int32_t tag_id = (int32_t)(TagId -> valuedouble);
        int offset = (int)(Offset -> valuedouble);
        int length = (int)(Length -> valuedouble);
        uint8_t* buffer = (uint8_t*)malloc(length + 1);
        int status = plc_tag_get_raw_bytes(tag_id, offset, buffer, length);
        if (status >= 0) {
            buffer[length] = '\0';
            result[index] = (char*)malloc(length / 2 * 3 + 1);
            base64_encode(buffer, length, result[index]);
            free(buffer);
        }
    }
    cJSON* response = cJSON_CreateStringArray((const char *const *)result, size);

    for (index = 0; index < size; index++) {
        free(result[index]);
    }
    free(result);
    return response;
}

cJSON* ethernet_ip_write(cJSON* request, char **error) {
    // cJSON* TagId = cJSON_GetObjectItemCaseSensitive(request, "tag_id");
    // cJSON* Offset = cJSON_GetObjectItemCaseSensitive(request, "offset");
    // cJSON* Length = cJSON_GetObjectItemCaseSensitive(request, "length");
    // cJSON* Value = cJSON_GetObjectItemCaseSensitive(request, "value");
    // cJSON* response = NULL;

    // int32_t tag_id = (int32_t)(TagId -> valuedouble);
    // int offset = (int)(Offset -> valuedouble);
    // int length = (int)(Length -> valuedouble);
    // uint8_t data[10];
    // int data_len = base64_decode(Value -> valuestring, length, data);
    // int status = plc_tag_set_raw_bytes(tag_id, offset, data, data_len);
    // if (status < 0) {
    //     response = on_error("Cannot write data to tag");
    //     return response;
    // }
    // plc_tag_write(tag_id, TIMEOUT);
    // response = cJSON_CreateString("ok");
    return cJSON_CreateString("ok");
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
            }
        }
        if (!done) {
            // sleep one ms
            usleep(1000);
        }
    }
    return;
}

int main(int argc, char* argv[]) {

    eport_loop(&on_request);
    return 0;
}