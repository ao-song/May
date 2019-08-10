// Copyright 2019 Ao Song <ao.song@outlook.com>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

 #include <iostream>
 #include <json.hpp>
 
 using json = nlohmann::json;
 
 int main()
 {
     // create a JSON object
     json j_object = json::parse("[{\"one\": 1}, {\"two\": 2}]");
 
     // call find
     auto it_two = j_object.find("two");
     auto it_three = j_object.find("three");
 
     // print values
     std::cout << std::boolalpha;
     std::cout << "\"two\" was found: " << (it_two != j_object.end()) << '\n';
     std::cout << "\"three\" was found: " << (it_three != j_object.end()) << '\n';

     std::cout << j_object[0]["one"];
 }