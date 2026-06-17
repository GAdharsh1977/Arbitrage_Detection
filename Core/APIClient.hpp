#pragma once
#include <string>
#include <vector>

namespace APIClient {
    // Fetches currency exchange rates from the Frankfurter API relative to INR base currency
    std::string fetchRatesWithAPI(const std::vector<std::string> &targets, const std::string &cacert_path);
}
