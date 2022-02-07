#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <eport.h>
#include <base64.h>
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
    int status = plc_tag_destroy((int32_t)(request -> valuedouble));
    if (status < 0) {
        return on_error("Cannot destroy tag");   
    }
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
    uint8_t* buffer = (uint8_t*)malloc(length + 1);
    int status = plc_tag_get_raw_bytes(tag_id, offset, buffer, length);
    if (status < 0) {
        response = on_error("Cannot read data form tag");
        return response;
    }
    buffer[length] = '\0';
    uint8_t* encoded_str = (uint8_t*)malloc(length / 2 * 3 + 1);
    base64_encode(buffer, length, encoded_str);
    response = cJSON_CreateString(encoded_str);
    free(buffer);
    free(encoded_str);
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
    char* tag_string_base = request -> valuestring;
    int status = 0;
    cJSON* tag_info = browse_tags(tag_string_base, &status);
    if (status == 0) {
        return on_ok(tag_info);
    } else {
        cJSON *error = on_error(tag_info -> valuestring);
        cJSON_Delete(tag_info);
        return error;
    }
    
}

int main(int argc, char* argv[]) {

    eport_loop(&on_request);
    return 0;
}