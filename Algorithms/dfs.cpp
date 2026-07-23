#include "dfs.hpp"
#include "../Core/GraphUtils.hpp"
#include <stack>
#include <unordered_set>
#include <algorithm>
#include <iostream>

namespace Algorithms {

std::vector<std::vector<int>> dfs(std::vector<std::vector<std::pair<double, int>>> &adjacency, int max_len, int source) {
    //pruning unreachable nodes
    adjacency = GraphUtils::remove_edges(adjacency, source);
    
    std::vector<std::vector<int>> all_cycles;
    std::stack<int> dfs_stack;
    std::vector<int> path;
    std::unordered_set<int> inPath;

    int N = adjacency.size();
    if (source < 0 || source >= N)
        return all_cycles;

    dfs_stack.push(source);

    while (!dfs_stack.empty()) {
        int node = dfs_stack.top();
        dfs_stack.pop();

        if (node == -1) { //backtrack marker
            if (!path.empty()) {
                int last = path.back();
                path.pop_back();
                inPath.erase(last);
            }
            continue;
        }

        if (node < 0 || node >= N)
            continue;

        if (inPath.count(node)) {
            auto it = std::find(path.begin(), path.end(), node);
            if (it != path.end()) {
                std::vector<int> cycle(it, path.end());
                cycle.push_back(node);
                if (cycle.size() >= 2) { //cycle must have >= 2 nodes.
                    all_cycles.push_back(cycle);
                }
            }
            continue;
        }

        path.push_back(node);
        inPath.insert(node);
        dfs_stack.push(-1);

        for (auto it = adjacency[node].rbegin(); it != adjacency[node].rend(); ++it) {
            int neighbor = it->second;
            if (neighbor >= N || neighbor < 0)
                continue;
            if (static_cast<int>(path.size()) >= max_len)
                continue;

            if (inPath.count(neighbor)) {
                auto it2 = std::find(path.begin(), path.end(), neighbor);
                if (it2 != path.end()) {
                    std::vector<int> cycle(it2, path.end());
                    cycle.push_back(neighbor);
                    all_cycles.push_back(cycle);
                }
            } else {
                dfs_stack.push(neighbor);
            }
        }
    }
    return all_cycles;
}

std::pair<std::vector<std::vector<int>>, int> filter_negative_cycles_dfs(
    std::vector<std::vector<int>> &cycles,
    const std::vector<std::vector<std::pair<double, int>>> &adjacency,
    std::vector<double> &weights,
    double transaction_fee) {
    
    std::vector<std::vector<int>> negative_cycles;
    std::vector<double> neg_profits;

    for (size_t i = 0; i < cycles.size(); i++) {
        if (cycles[i].size() < 2)
            continue;

        //check the presence of INR
        bool has_inr = false;
        for (int node : cycles[i]) {
            if (node == 0) {
                has_inr = true;
                break;
            }
        }
        if (!has_inr)
            continue;

        double amount = 1.0;
        bool invalid = false;

        for (size_t j = 0; j + 1 < cycles[i].size(); j++) {
            int u = cycles[i][j];
            int v = cycles[i][j + 1];

            if (u < 0 || u >= static_cast<int>(adjacency.size()) ||
                v < 0 || v >= static_cast<int>(adjacency.size())) {
                invalid = true;
                break;
            }

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
            amount *= (1.0 - transaction_fee);
            if (amount <= 0) {
                invalid = true;
                break;
            }
        }

        if (invalid)
            continue;

        if (amount > 1.005) {
            negative_cycles.push_back(cycles[i]);
            neg_profits.push_back(amount);
        }
    }

    int best_idx = -1;
    if (!neg_profits.empty()) {
        best_idx = 0;
        for (size_t k = 1; k < neg_profits.size(); ++k) {
            if (neg_profits[k] > neg_profits[best_idx])
                best_idx = k;
        }
    }

    return std::make_pair(negative_cycles, best_idx);
}

}
