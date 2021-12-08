#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <eport.h>
#include "ethernet_ip.h"

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


cJSON* ethernet_ip_create_tag(cJSON* request) {

}



int main(int argc, char* argv[]) {

    eport_loop(&on_request);
    return 0;
}