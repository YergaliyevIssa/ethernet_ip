#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <eport.h>
#include "ethernet_ip.h"


int parse_create_request( cJSON* body );
int parse_read_request( cJSON* body );
int parse_write_request( cJSON* body );
int parse_destroy_request( cJSON* body);


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
    }else if( request->cmd == CREATE_TAG ){
        if (parse_create_request( request->body ) != 0){ goto error; }
    }else if( request->cmd == DESTROY_TAG ){
        if (parse_destroy_request( request->body ) != 0){ goto error; }
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


int parse_create_request(cJSON* body){
    return 0;
}

