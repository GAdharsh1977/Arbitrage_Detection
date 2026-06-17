#include "bellman_ford.hpp"
#include "../Core/GraphUtils.hpp"
#include <algorithm>
#include <cmath>
#include <unordered_set>
#include <iostream>

namespace Algorithms {

std::pair<std::vector<double>, std::vector<int>> bellmanFord(std::vector<std::vector<std::pair<double, int>>> &adjacency, int source) {
    GraphUtils::logarithm(adjacency);
    int n = adjacency.size();
    std::vector<double> distances(n, 1e9);
    std::vector<int> parent(n, -1);
    
    if (source >= 0 && source < n) {
        distances[source] = 0.0;
    }

    for (int i = 0; i < n - 1; i++) {
        for (int j = 0; j < n; j++) {
            for (const auto &k : adjacency[j]) {
                if (distances[j] != 1e9 && distances[k.second] > distances[j] + k.first) {
                    distances[k.second] = distances[j] + k.first;
                    parent[k.second] = j;
                }
            }
        }
    }

    return std::make_pair(distances, parent);
}

std::vector<std::vector<int>> find_negative_cycles_bf(const std::vector<std::vector<std::pair<double, int>>> &adjacency, int source, double fee) {
    // Work on a copy so we don't destroy the original rates matrix
    std::vector<std::vector<std::pair<double, int>>> adj_copy = adjacency;
    auto [distances, parent] = bellmanFord(adj_copy, source);

    int n = adj_copy.size();
    
    // Find nodes affected by negative cycles
    std::unordered_set<int> affected;
    for (int j = 0; j < n; j++) {
        for (const auto &k : adj_copy[j]) {
            if (distances[j] != 1e9 && distances[k.second] > distances[j] + k.first) {
                affected.insert(k.second);
            }
        }
    }

    // Backtrack N times to guarantee we land inside the cycle
    std::vector<int> cycle_nodes(affected.begin(), affected.end());
    for (int i = 0; i < n; i++) {
        for (int &node : cycle_nodes) {
            if (node != -1) {
                node = parent[node];
            }
        }
    }

    // Remove duplicates and invalid parent entries (-1)
    std::sort(cycle_nodes.begin(), cycle_nodes.end());
    cycle_nodes.erase(std::unique(cycle_nodes.begin(), cycle_nodes.end()), cycle_nodes.end());
    cycle_nodes.erase(std::remove(cycle_nodes.begin(), cycle_nodes.end(), -1), cycle_nodes.end());

    // Reconstruct cycles from parent pointers
    std::vector<std::vector<int>> all_cycles;
    for (int node : cycle_nodes) {
        std::vector<int> cycle;
        std::unordered_set<int> visited;
        int cur = node;

        while (visited.find(cur) == visited.end() && cur != -1) {
            cycle.push_back(cur);
            visited.insert(cur);
            cur = parent[cur];
        }
        if (!cycle.empty()) {
            cycle.push_back(cycle.front());
            std::reverse(cycle.begin(), cycle.end());
        }

        // Normalize cycle: rotate so that the smallest element (index) is first
        if (cycle.size() > 1) {
            cycle.pop_back();
            std::rotate(cycle.begin(), std::min_element(cycle.begin(), cycle.end()), cycle.end());
            cycle.push_back(cycle.front());
        }

        all_cycles.push_back(cycle);
    }

    // Deduplicate the list of cycles
    std::sort(all_cycles.begin(), all_cycles.end());
    all_cycles.erase(std::unique(all_cycles.begin(), all_cycles.end()), all_cycles.end());

    // Filter - only keep cycles containing INR (0) that are profitable after fees
    std::vector<std::vector<int>> result;
    for (const auto &cycle : all_cycles) {
        if (cycle.size() < 2) continue;

        // Must contain INR (0)
        bool has_inr = false;
        for (int node : cycle) {
            if (node == 0) {
                has_inr = true;
                break;
            }
        }
        if (!has_inr) continue;

        // Simulate the path with original rates (adjacency) + fees
        double amount = 1.0;
        bool invalid = false;
        for (size_t i = 0; i + 1 < cycle.size(); i++) {
            int u = cycle[i], v = cycle[i + 1];
            double rate = -1.0;
            for (const auto &edge : adjacency[u]) {
                if (edge.second == v) {
                    rate = edge.first;
                    break;
                }
            }
            if (rate < 0) {
                invalid = true;
                break;
            }
            amount *= rate;
            amount *= (1.0 - fee);
            if (amount <= 0) {
                invalid = true;
                break;
            }
        }

        if (!invalid && amount > 1.005) {
            result.push_back(cycle);
        }
    }
    return result;
}

}
