#include "GraphUtils.hpp"
#include "json.hpp"
#include <cmath>
#include <queue>
#include <unordered_set>
#include <algorithm>
#include <iostream>
#include <cstdlib>

using json = nlohmann::json;

namespace GraphUtils {

void logarithm(std::vector<std::vector<std::pair<double, int>>> &adjacency) {
    for (auto &v : adjacency) {
        for (auto &p : v) {
            p.first = -std::log(p.first);
        }
    }
}

std::vector<bool> eliminate_unnecessary(const std::vector<std::vector<std::pair<double, int>>> &adjacency, int source) {
    int N = adjacency.size();
    std::vector<bool> reachable(N, false);
    std::queue<int> q;

    if (source >= 0 && source < N) {
        reachable[source] = true;
        q.push(source);
    }

    while (!q.empty()) {
        int u = q.front();
        q.pop();

        for (const auto &edge : adjacency[u]) {
            int v = edge.second;
            if (v >= 0 && v < N && !reachable[v]) {
                reachable[v] = true;
                q.push(v);
            }
        }
    }
    return reachable;
}

std::vector<std::vector<std::pair<double, int>>> remove_edges(std::vector<std::vector<std::pair<double, int>>> &adjacency, int source) {
    std::vector<bool> reachable = eliminate_unnecessary(adjacency, source);
    int N = adjacency.size();
    for (int i = 0; i < N; i++) {
        if (reachable[i]) {
            auto &edges = adjacency[i];
            edges.erase(std::remove_if(edges.begin(), edges.end(),
                                  [&](const std::pair<double, int> &p) {
                                      return (p.second >= N || p.second < 0 ||
                                              (!reachable[p.second] && p.second != source)); // keep edges to source
                                  }),
                        edges.end());
        }
    }
    return adjacency;
}

void normalise_cycles(const std::vector<std::vector<std::pair<double, int>>> &adjacency, 
                      std::vector<std::vector<int>> &cycles, 
                      std::vector<double> &weights) {
    for (auto &c : cycles) {
        if (c.empty())
            continue;
        c.pop_back();
        std::rotate(c.begin(), std::min_element(c.begin(), c.end()), c.end());
    }

    std::unordered_set<size_t> unique_hashes;
    std::vector<std::vector<int>> deduped;
    for (auto &cycle : cycles) {
        size_t h = 0;
        for (int x : cycle)
            h = h * 31 + static_cast<size_t>(x);
        if (unique_hashes.insert(h).second)
            deduped.push_back(cycle);
    }
    cycles = std::move(deduped);

    // completing the cycle (pushing the first element of the cycle)
    for (size_t i = 0; i < cycles.size(); ++i) {
        if (!cycles[i].empty())
            cycles[i].push_back(cycles[i].front());
    }
    weights.assign(cycles.size(), 0.0);
}

std::vector<std::vector<std::pair<double, int>>> buildAdj(const std::string &json_str, const std::vector<std::string> &currencies) {
    int n = currencies.size();
    std::vector<std::vector<std::pair<double, int>>> adj(n);

    try {
        auto parsed = json::parse(json_str);
        if (!parsed.contains("base") || parsed["base"] != "INR") {
            std::cerr << "INR is not the base." << std::endl;
            return adj;
        }

        auto rates_obj = parsed["rates"];
        if (rates_obj.is_null()) {
            std::cerr << "No rates found in JSON response." << std::endl;
            return adj;
        }

        std::vector<double> rates(n);
        rates[0] = 1.0; // INR base rate

        for (int i = 1; i < n; i++) {
            std::string cur = currencies[i];
            if (rates_obj.contains(cur))
                rates[i] = rates_obj[cur].get<double>();
            else
                rates[i] = 0.01; // fallback rate
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i == j)
                    continue;
                // Add minor noise simulation (±3% per edge)
                double noise = 1.0 + ((std::rand() % 600) - 300) * 0.0001;
                double w = (rates[j] / rates[i]) * noise;
                adj[i].emplace_back(std::make_pair(w, j));
            }
        }
        return adj;
    }
    catch (const std::exception &e) {
        std::cerr << "JSON Parse error: " << e.what() << std::endl;
        return adj;
    }
}

std::vector<std::vector<std::pair<double, int>>> buildSimulatedAdj(const std::vector<std::string> &currencies) {
    int n = currencies.size();
    std::vector<std::vector<std::pair<double, int>>> adj(n);

    
    std::vector<double> rates = {
        1.0,      // INR
        0.01182,  // USD
        1.7823,   // JPY
        0.01842,  // AUD
    };

    // Calculate baseline conversion rates with a default suppression factor (0.1)
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == j) continue;
            adj[i].emplace_back((rates[j] / rates[i]) * 0.1, j);
        }
    }

    // Lambda helper to update a rate between two currencies
    auto setRate = [&](int from, int to, double rate) {
        for (auto &e : adj[from]) {
            if (e.second == to) {
                e.first = rate;
                return;
            }
        }
    };

    if (n >= 4) {
        setRate(0, 1, (rates[1] / rates[0]) * 1.5); // INR -> USD
        setRate(1, 2, (rates[2] / rates[1]) * 1.5); // USD -> JPY
        setRate(2, 3, (rates[3] / rates[2]) * 1.5); // JPY -> AUD
        setRate(3, 0, (rates[0] / rates[3]) * 1.5); // AUD -> INR
    }

    return adj;
}

} // namespace GraphUtils
