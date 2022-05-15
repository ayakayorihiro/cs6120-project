#include <iostream>
#include "json/include/nlohmann/json.hpp"

using json = nlohmann::json;

int main()
{

  std::ifstream i("yo.json");
  json j;
  i >> j;

  std::cout << j.dump(4) << std::endl;

  return 0;
}
