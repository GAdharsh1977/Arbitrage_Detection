#pragma once
#include <string>
#include <vector>

namespace APIClient {
    //fetch currency exchange rates using API.
    std::string fetchRatesWithAPI(const std::vector<std::string> &targets, const std::string &cacert_path);
}
