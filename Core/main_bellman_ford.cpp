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
#include "GraphUtils.hpp"
#include "../Algorithms/bellman_ford.hpp"

int main() {
    std::srand(std::time(nullptr));
    const double fee = 0.01;
    int iteration = 0;

    // Create thread pool
    ThreadPool pool(std::thread::hardware_concurrency() - 1);
    
    // Build simulated adjacency matrix
    auto adj = GraphUtils::buildSimulatedAdj(Config::sim_currencies);
    ArbitrageEngine engine(adj.size(), 1.0, fee, 0, 0.5, Config::sim_currencies);

    while (true) {
        iteration++;
        std::cout << "iteration: " << iteration << "\n";

        // Re-generate simulated graph
        adj = GraphUtils::buildSimulatedAdj(Config::sim_currencies);

        // Parallel Bellman-Ford negative cycle finder
        auto cycles_future = pool.enqueue([&]() { 
            return Algorithms::find_negative_cycles_bf(adj, 0, fee); 
        });
        auto cycles = cycles_future.get();

        std::cout << "Profitable INR cycles found: " << cycles.size() << "\n";

        if (!cycles.empty()) {
            // Pick the most profitable cycle
            int best = 0;
            double best_profit = 0.0;
            for (int i = 0; i < (int)cycles.size(); i++) {
                double amount = 1.0;
                for (int j = 0; j + 1 < (int)cycles[i].size(); j++) {
                    for (const auto &e : adj[cycles[i][j]]) {
                        if (e.second == cycles[i][j+1]) { 
                            amount *= e.first * (1.0 - fee); 
                            break; 
                        }
                    }
                }
                if (amount > best_profit) { 
                    best_profit = amount; 
                    best = i; 
                }
            }

            auto &chosen = cycles[best];
            for (size_t i = 0; i < chosen.size(); i++) {
                std::cout << Config::sim_currencies[chosen[i]];
                if (i < chosen.size() - 1) 
                    std::cout << " -> ";
            }
            std::cout << "\n";

            if (engine.getBalance(0) > 0.01)
                engine.execute_cycle(adj, chosen, 1);
            else
                std::cout << "Insufficient INR balance\n";

            engine.print_balances();
        } else {
            std::cout << "No profitable cycles found\n";
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(1000));
        std::cout << std::endl;
    }
    return 0;
}
