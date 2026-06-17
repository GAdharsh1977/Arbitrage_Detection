#include <iostream>
#include <vector>
#include <string>
#include <chrono>
#include <thread>
#include <cstdlib>
#include <ctime>

#include "Config.hpp"
#include "ThreadPool.hpp"
#include "ArbitrageEngine.hpp"
#include "APIClient.hpp"
#include "GraphUtils.hpp"
#include "../Algorithms/dfs.hpp"

int main() {
    std::srand(std::time(nullptr));
    int iteration = 0;
    const double fee = 0.01;
    
    // Create thread pool
    ThreadPool pool(std::thread::hardware_concurrency() - 1);

    // Initial rates fetch to size the arbitrage engine
    std::string json_str = APIClient::fetchRatesWithAPI(Config::market_targets, Config::cacert);
    auto adj = GraphUtils::buildAdj(json_str, Config::market_currencies);
    
    if (adj.empty()) {
        std::cerr << "Initial rates fetch failed. Adjacency matrix is empty. Please check internet connection or API status.\n";
        return 1;
    }

    ArbitrageEngine engine(adj.size(), 1.0, fee, 0, 0.5, Config::market_currencies);

    while (true) {
        iteration++;
        std::cout << "iteration: " << iteration << "\n";

        // Re-fetch exchange rates each iteration
        json_str = APIClient::fetchRatesWithAPI(Config::market_targets, Config::cacert);
        adj = GraphUtils::buildAdj(json_str, Config::market_currencies);
        
        if (adj.empty() || adj[0].empty()) {
            std::cerr << "Adj empty; check API connection." << std::endl;
            std::this_thread::sleep_for(std::chrono::milliseconds(2000));
            continue;
        }

        // Parallel DFS cycle discovery
        auto cycles_future = pool.enqueue([&]() { 
            return Algorithms::dfs(adj, 5, 0); 
        });
        auto cycles = cycles_future.get();

        size_t matrix_size = adj.size();
        std::vector<double> weights(matrix_size, 0.0);
        
        // Parallel cycle normalization and deduplication
        pool.enqueue([&]() { 
            GraphUtils::normalise_cycles(adj, cycles, weights); 
        }).get();

        // Parallel cycle profitability calculation
        auto neg_cycles_future = pool.enqueue([&]() { 
            return Algorithms::filter_negative_cycles_dfs(cycles, adj, weights, fee); 
        });
        auto neg_cycles = neg_cycles_future.get();
        
        std::vector<std::vector<int>> negative_cycles = neg_cycles.first;
        int best_index = neg_cycles.second;

        std::cout << "Profitable INR cycles found: " << negative_cycles.size() << "\n";

        if (!negative_cycles.empty() && best_index >= 0) {
            std::vector<int> chosen_cycle = negative_cycles[best_index];

            for (size_t i = 0; i < chosen_cycle.size(); i++) {
                std::cout << Config::market_currencies[chosen_cycle[i]];
                if (i < chosen_cycle.size() - 1)
                    std::cout << " -> ";
            }
            std::cout << "\n";

            if (engine.getBalance(0) > 0.01) {
                engine.execute_cycle(adj, chosen_cycle, 1);
            } else {
                std::cout << "Insufficient INR balance\n";
            }
        } else {
            std::cout << "No profitable cycles found this iteration\n";
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(1000));
        std::cout << std::endl;
    }
    return 0;
}
