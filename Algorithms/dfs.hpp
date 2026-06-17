#pragma once
#include <vector>
#include <utility>

namespace Algorithms {
    // DFS cycle detection starting from a specific source currency
    std::vector<std::vector<int>> dfs(std::vector<std::vector<std::pair<double, int>>> &adjacency, int max_len, int source);

    // Filters cycles found by DFS to find profitable ones after transaction fees
    std::pair<std::vector<std::vector<int>>, int> filter_negative_cycles_dfs(
        std::vector<std::vector<int>> &cycles,
        const std::vector<std::vector<std::pair<double, int>>> &adjacency,
        std::vector<double> &weights,
        double transaction_fee_pc = 0.01);
}
