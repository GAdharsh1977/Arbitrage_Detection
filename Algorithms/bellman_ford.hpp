#pragma once
#include <vector>
#include <utility>

namespace Algorithms {
    // Bellman-Ford shortest paths on negative logarithms
    std::pair<std::vector<double>, std::vector<int>> bellmanFord(std::vector<std::vector<std::pair<double, int>>> &adjacency, int source);

    // Identifies negative cycle loops starting from/containing INR (0) using Bellman-Ford
    std::vector<std::vector<int>> find_negative_cycles_bf(const std::vector<std::vector<std::pair<double, int>>> &adjacency, int source, double fee);
}
