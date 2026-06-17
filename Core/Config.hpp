#pragma once
#include <vector>
#include <string>

namespace Config {
    // Path to the CA certificate bundle (used by curl for SSL verification)
    inline const std::string cacert = "Your cacert's path";

    // Currencies used for the real-market API (Frankfurter)
    inline const std::vector<std::string> market_currencies = {
        "INR", "USD", "EUR", "GBP", "JPY", "AUD", "CAD", "CHF", "TRY", "ZAR",
        "CNY", "HKD", "SGD", "NZD", "MXN", "KRW", "BRL", "RUB", "SEK", "NOK",
        "PLN", "DKK", "MYR", "THB", "ILS"
    };

    // Target currencies to fetch rates for
    inline const std::vector<std::string> market_targets = {
        "AUD", "USD", "EUR", "GBP", "JPY", "CAD", "CHF", "TRY", "ZAR",
        "CNY", "HKD", "SGD", "NZD", "MXN", "KRW", "BRL", "RUB", "SEK", "NOK",
        "PLN", "DKK", "MYR", "THB", "ILS"
    };

    // Currencies used for the simulated Bellman-Ford graph
    inline const std::vector<std::string> sim_currencies = {
        "INR", "USD", "JPY", "AUD"
    };
}
