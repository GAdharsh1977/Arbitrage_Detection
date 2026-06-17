#pragma once
#include <vector>
#include <string>
#include <utility>

namespace GraphUtils {
    // Converts edge weights to their negative log values
    void logarithm(std::vector<std::vector<std::pair<double, int>>> &adjacency);

    // Determines reachable nodes from the source currency using BFS
    std::vector<bool> eliminate_unnecessary(const std::vector<std::vector<std::pair<double, int>>> &adjacency, int source);

    // Prunes edges going to unreachable nodes
    std::vector<std::vector<std::pair<double, int>>> remove_edges(std::vector<std::vector<std::pair<double, int>>> &adjacency, int source);

    // Normalizes, rotates, and deduplicates cycles
    void normalise_cycles(const std::vector<std::vector<std::pair<double, int>>> &adjacency, 
                          std::vector<std::vector<int>> &cycles, 
                          std::vector<double> &weights);

    // Builds the adjacency matrix from the Frankfurter JSON rates
    std::vector<std::vector<std::pair<double, int>>> buildAdj(const std::string &json_str, const std::vector<std::string> &currencies);

    // Builds a simulated adjacency matrix with a hardcoded, boosted arbitrage cycle
    std::vector<std::vector<std::pair<double, int>>> buildSimulatedAdj(const std::vector<std::string> &currencies);
}
