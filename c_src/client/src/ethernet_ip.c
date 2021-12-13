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
    cJSON* response;
    char* connection_path = request -> valuestring;
    int32_t tag_id = plc_tag_create(connection_path, 5000);
    return cJSON_CreateNumber(tag_id);
}


cJSON* ethernet_ip_destroy_tag(cJSON* request) {
    plc_tag_destroy((int32_t)(request -> valuedouble));
    return NULL;
}

cJSON* ethernet_ip_read(cJSON* request) {
    cJSON* TagId = cJSON_GetObjectItemCaseSensitive(request, "tag_id");
    cJSON* Type = cJSON_GetObjectItemCaseSensitive(request,"type");
    cJSON* Offset = cJSON_GetObjectItemCaseSensitive(request, "offset");
    cJSON* response = NULL;
    int32_t tag_id = (int32_t)(TagId -> valuedouble);
    char* type = Type -> valuestring;
    int offset = (int)(Offset -> valuedouble);
    if (strcmp(type, "uint64") == 0) {
        uint64_t value = plc_tag_get_uint64(tag_id, offset);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "int64") == 0) {
        int64_t value = plc_tag_get_int64(tag_id, offset);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "uint32") == 0) {
        uint32_t value = plc_tag_get_uint32(tag_id, offset);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "int32") == 0) {
        int32_t value = plc_tag_get_int32(tag_id, offset);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "uint16") == 0) {
        uint16_t value = plc_tag_get_uint16(tag_id, offset);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "int16") == 0) {
        int16_t value = plc_tag_get_int16(tag_id, offset);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "uint8") == 0) {
        uint8_t value = plc_tag_get_uint16(tag_id, offset);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "int8") == 0) {
        int8_t value = plc_tag_get_uint16(tag_id, offset);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "float64") == 0) {
        double value = plc_tag_get_float64(tag_id, offset);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "float32") == 0) {
        float value = plc_tag_get_float32(tag_id, offset);
        response = cJSON_CreateNumber(value);
    } else {
        response = on_error(type);
        return response;
    }
    
    return on_ok(response);
}

cJSON* ethernet_ip_write(cJSON* request) {
    cJSON* TagId = cJSON_GetObjectItemCaseSensitive(request, "tag_id");
    cJSON* Type = cJSON_GetObjectItemCaseSensitive(request,"type");
    cJSON* Offset = cJSON_GetObjectItemCaseSensitive(request, "offset");
    cJSON* Value = cJSON_GetObjectItemCaseSensitive(request, "value");

    int32_t tag_id = (int32_t)(TagId -> valuedouble);
    char* type = Type -> valuestring;
    int offset = (int)(Offset -> valuedouble);
    double value = Value -> valuedouble;
    int ret_value;
    if (strcmp(type, "uint64") == 0) {
        ret_value = plc_tag_set_uint64(tag_id, offset, value);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "int64") == 0) {
        ret_value = plc_tag_set_int64(tag_id, offset, value);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "uint32") == 0) {
        ret_value = plc_tag_set_uint32(tag_id, offset, value);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "int32") == 0) {
        ret_value = plc_tag_set_int32(tag_id, offset, value);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "uint16") == 0) {
        ret_value = plc_tag_set_uint16(tag_id, offset, value);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "int16") == 0) {
        ret_value = plc_tag_set_int16(tag_id, offset, value);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "uint8") == 0) {
        ret_value = plc_tag_set_uint16(tag_id, offset,value);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "int8") == 0) {
        ret_value = plc_tag_set_uint16(tag_id, offset, value);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "float64") == 0) {
        ret_value = plc_tag_set_float64(tag_id, offset, value);
        response = cJSON_CreateNumber(value);
    } else if (strcmp(type, "float32") == 0) {
        ret_value = plc_tag_set_float32(tag_id, offset, value);
        response = cJSON_CreateNumber(value);
    } else {
        response = on_error(type);
        return response;
    }

    return
}


int main(int argc, char* argv[]) {

    eport_loop(&on_request);
    return 0;
}