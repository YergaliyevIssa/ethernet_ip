/* Copyright (c) 2021 Faceplate
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
#include <cjson/cJSON.h>
#include <libplctag.h>

// Timeout for reading and writing operations
#define TIMEOUT 5000

// debug print
//#define DEBUG_PRINT 1
#ifdef DEBUG_PRINT
#define LOGDEBUG(...) do{ fprintf(stdout,__VA_ARGS__); } while(0)
#else
#define LOGDEBUG(...) do{  } while(0)
#endif


typedef unsigned long TID;

// Command types
typedef enum ETHERNET_IP_CMD {
    CREATE_TAG,
    DESTROY_TAG,
    READ_DATA,
    WRITE_DATA,
    BROWSE_TAGS
} ETHERNET_IP_CMD;

// Request
typedef struct ethernet_ip__client_request{
    ETHERNET_IP_CMD cmd;
    TID tid;
    cJSON *body;
} ETHERNET_IP_CLIENT_REQUEST;

// Parse a request
int parse_request( const char *message, ETHERNET_IP_CLIENT_REQUEST* request );
void purge_request( ETHERNET_IP_CLIENT_REQUEST *request );

// Build response
char* create_response( ETHERNET_IP_CLIENT_REQUEST *request, cJSON *response );

// source discovery
// we get all necessary information about all tags in AB PLC
cJSON* browse_tags(const char* tag_string_base, int* status);