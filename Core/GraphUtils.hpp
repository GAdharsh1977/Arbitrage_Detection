#pragma once
#include <vector>
#include <string>
#include <utility>

namespace GraphUtils {
    //convert weights to log values.
    void logarithm(std::vector<std::vector<std::pair<double, int>>> &adjacency);

    //eliminate unreachable nodes from the source
    std::vector<bool> eliminate_unnecessary(const std::vector<std::vector<std::pair<double, int>>> &adjacency, int source);

    std::vector<std::vector<std::pair<double, int>>> remove_edges(std::vector<std::vector<std::pair<double, int>>> &adjacency, int source);

    //normalize, rotate, and deduplicate cycles.
    void normalise_cycles(const std::vector<std::vector<std::pair<double, int>>> &adjacency, 
                          std::vector<std::vector<int>> &cycles, 
                          std::vector<double> &weights);

    //builds adjacency matrix from rates given by API.
    std::vector<std::vector<std::pair<double, int>>> buildAdj(const std::string &json_str, const std::vector<std::string> &currencies);

    std::vector<std::vector<std::pair<double, int>>> buildSimulatedAdj(const std::vector<std::string> &currencies);
}
