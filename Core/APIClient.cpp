#include "APIClient.hpp"
#include <iostream>
#include <curl/curl.h>

namespace APIClient {

static size_t writeCallBack(void *contents, size_t size, size_t nmemb, std::string *userp) {
    size_t realSize = size * nmemb;
    userp->append(static_cast<char*>(contents), realSize);
    return realSize;
}

std::string fetchRatesWithAPI(const std::vector<std::string> &targets, const std::string &cacert_path) {
    CURL *curl = curl_easy_init();
    if (!curl) {
        std::cerr << "Curl not initialised." << std::endl;
        return "";
    }

    std::string Additions;
    for (const auto &currency : targets) {
        if (currency != "INR") {
            if (!Additions.empty()) {
                Additions += ",";
            }
            Additions += currency;
        }
    }

    if (Additions.empty()) {
        std::cout << "No targets given." << std::endl;
        curl_easy_cleanup(curl);
        return "";
    }

    std::string url = "https://api.frankfurter.app/latest?from=INR&to=" + Additions;

    std::string readBuffer;
    curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 10L);
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallBack);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

    // Apply certificate path from config if provided
    if (!cacert_path.empty() && cacert_path != "Your cacert's path") {
        curl_easy_setopt(curl, CURLOPT_CAINFO, cacert_path.c_str());
        curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 2L);
        curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 2L);
    } else {
        // If certificate path is the default placeholder, disable peer verification to prevent SSL error 60 on Windows
        curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
        curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0L);
    }

    CURLcode res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        std::cerr << "Fetch failed: " << curl_easy_strerror(res) << " (Error code: " << res << ")" << std::endl;
        return "";
    }
    if (readBuffer.empty()) {
        std::cout << "Void response." << std::endl;
        return "";
    }
    if (readBuffer.find("\"base\":\"INR\"") == std::string::npos) {
        std::cerr << "API error: Base currency mismatch or rate error." << std::endl;
        return "";
    }
    return readBuffer;
}

} // namespace APIClient
