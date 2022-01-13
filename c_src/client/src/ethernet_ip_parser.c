#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <eport.h>
#include "ethernet_ip.h"


int parse_create_request( cJSON* body );
int parse_read_request( cJSON* body );
int parse_write_request( cJSON* body );
int parse_destroy_request( cJSON* body);

ETHERNET_IP_CMD string2cmd(char *cmd);
char* cmd2string(ETHERNET_IP_CMD cmd);


int parse_request( const char *message, ETHERNET_IP_CLIENT_REQUEST* request ){

    cJSON *JSON = NULL;
    const cJSON *cmd = NULL;
    const cJSON *tid = NULL;
    request->body = NULL;

    // Parse the request to JSON structure
    LOGDEBUG("DEBUG: parse JSON\r\n");
    JSON = cJSON_Parse( message );
    if (JSON == NULL){
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            LOGERROR("ERROR: invalid JSON before: %s\r\n", error_ptr);
        }else{
            LOGERROR("ERROR: invalid JSON\r\n");
        }
        goto error;
    }

    // Parse the type of the request
    LOGDEBUG("DEBUG: parse cmd\r\n");
    cmd = cJSON_GetObjectItemCaseSensitive(JSON, "cmd");
    if (cJSON_IsString(cmd) && (cmd->valuestring != NULL)){
        request->cmd = string2cmd( cmd->valuestring );
        if (request->cmd == -1){
            LOGERROR("ERROR: invalid command type: %s\r\n", cmd->valuestring);
            goto error;
        }
    } else {
        LOGERROR("ERROR: command type is not defined\r\n");
        goto error;
    }

    // Parse transaction ID
    LOGDEBUG("DEBUG: parse tid\r\n");
    tid = cJSON_GetObjectItemCaseSensitive(JSON, "tid");
    if ( cJSON_IsNumber(tid)) {
        request->tid = tid->valuedouble;
    } else{
        LOGERROR("ERROR: transaction id not defined\r\n");
        goto error;
    }

    // Parse body
    LOGDEBUG("DEBUG: parse body\r\n");
    request->body = cJSON_DetachItemFromObject(JSON, "body");

    if (request->cmd == READ_DATA){
        if (parse_read_request( request->body ) != 0){ goto error; }
    } else if( request->cmd == WRITE_DATA ){
        if (parse_write_request( request->body ) != 0) { goto error; }
    } else if( request->cmd == CREATE_TAG ){
        if (parse_create_request( request->body ) != 0){ goto error; }
    } else if( request->cmd == DESTROY_TAG ){
        if (parse_destroy_request( request->body ) != 0){ goto error; }
    } else if (request->cmd == BROWSE_TAGS) {
        if (parse_browse_request( request->body ) != 0){ goto error; }
    } else {
        LOGERROR("ERROR: invalid command type %d\r\n",request->cmd);
        goto error;
    }

    if (request->body == NULL){
        LOGERROR("ERROR: unable to parse request body\r\n");
        goto error;
    }

    cJSON_Delete( JSON );

    return 0;

error:

    cJSON_Delete( JSON );
    return -1;
}


// Clean the memory used for the request structure
void purge_request( ETHERNET_IP_CLIENT_REQUEST* request ){
    cJSON_Delete( request->body );
}

// Build the response
char* create_response( ETHERNET_IP_CLIENT_REQUEST *request, cJSON *responseBody ){
    cJSON *response = cJSON_CreateObject();
    char *responseString = NULL;

    // Inherit command type from the request
    char *cmd = cmd2string( request->cmd );
    if (cmd == NULL){
        goto error;
    }
    if ( cJSON_AddStringToObject(response, "cmd", cmd) == NULL) {
        goto error;
    }

    // Inherit transaction id from the request
    if ( cJSON_AddNumberToObject(response, "tid", request->tid) == NULL) {
        goto error;
    } 

    // Add the response body
    if ( !cJSON_AddItemToObject(response, "reply", responseBody) ) {
        goto error;
    }
    responseString = cJSON_PrintUnformatted( response );
    if (responseString == NULL)
    {
        LOGERROR("ERROR: unable to print response.\r\n");
        goto error;
    }

    cJSON_Delete( response );
    return responseString;

error:
    cJSON_Delete( responseBody );
    cJSON_Delete( response );
    return "{\"type\":\"error\",\"text\":\"unable to construct the response\"}";
}



int parse_create_request(cJSON* body){
    if (!cJSON_IsString(body)) {
        return -1;
    }
    return 0;
}

int parse_destroy_request(cJSON* body) {
    if (!cJSON_IsNumber(body)) {
        return -1;
    }
    return 0;
}

int parse_read_request(cJSON* body) {
    return 0;
}

int parse_write_request(cJSON* body) {
    return 0;
}

int parse_browse_request(cJSON* body) {
    if (!cJSON_IsString(body)) {
        return -1;
    }
    return 0;
}

//------------------Internal helpers--------------------------------------------
ETHERNET_IP_CMD string2cmd(char *cmd){
    if ( strcmp(cmd, "create") == 0 ){
        return CREATE_TAG;
    } else if( strcmp(cmd, "destroy") == 0){
        return DESTROY_TAG;
    } else if( strcmp(cmd, "read") == 0){
        return READ_DATA;
    } else if( strcmp(cmd, "write") == 0){
        return WRITE_DATA;
    } else if (strcmp(cmd, "browse_tags") == 0) {
        return BROWSE_TAGS;
    } else{
        return -1; 
    }
}

char* cmd2string(ETHERNET_IP_CMD cmd){
    if ( cmd == CREATE_TAG ){
        return "create";
    } else if( cmd == DESTROY_TAG){
        return "destroy";
    } else if( cmd == READ_DATA){
        return "read";
    } else if( cmd == WRITE_DATA ){
        return "write";
    } else if (cmd == BROWSE_TAGS) {
        return "browse_tags";
    } else{
        return NULL; 
    }
}