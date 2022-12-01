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
// #include <cjson/cJSON.h>
#include <libplctag.h>
#include <eport_c.h>

// Timeout for reading and writing operations
#define TIMEOUT 5000

#define WRONG_TYPE_ERR -100

// Build response
// source discovery
// we get all necessary information about all tags in AB PLC
cJSON* browse_tags(char* tag_string_base, int* status);