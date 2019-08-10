// Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
// Distributed under the Apache License, Version 2.0 (https://www.apache.org/licenses/LICENSE-2.0)

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