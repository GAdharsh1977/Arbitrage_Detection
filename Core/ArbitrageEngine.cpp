#include "ArbitrageEngine.hpp"
#include <iostream>
#include <algorithm>

ArbitrageEngine::ArbitrageEngine(int size, double initial_capital, double transaction_fee_pct, int base, double traded_in_cycle, const std::vector<std::string>& currencies)
    : base_currency(base), transaction_fee_pc(transaction_fee_pct), traded_per_cycle(traded_in_cycle), currency_names(currencies) {
    balances.resize(size, 0.0);
    if (base >= 0 && base < size) {
        balances[base] = initial_capital;
    }
}

void ArbitrageEngine::deposit(int currency, double amount) {
    if (amount > 0 && currency >= 0 && currency < static_cast<int>(balances.size())) {
        balances[currency] += amount;
    } else if (currency < 0 || currency >= static_cast<int>(balances.size())) {
        std::cout << "Invalid index for deposit: " << currency << std::endl;
    }
}

double ArbitrageEngine::withdraw(int currency, double requested) {
    if (requested <= 0 || currency < 0 || currency >= static_cast<int>(balances.size())) {
        std::cout << "Invalid amount or index: " << requested << std::endl;
        return 0.0;
    }
    double available = balances[currency];
    if (requested > available) {
        std::cout << "Insufficient funds for " << currency_names[currency] << ": withdrawing all " << available << std::endl;
        balances[currency] = 0.0;
        return available;
    }
    balances[currency] -= requested;
    return requested;
}

void ArbitrageEngine::execute_exchange(int currency1, int currency2, double requested_amount, double rate) {
    double principal = withdraw(currency1, requested_amount);
    if (principal <= 0) return;

    double converted = principal * rate;
    converted -= converted * transaction_fee_pc;
    converted = std::max(0.0, converted); // prevent negative values

    std::cout << "Exchange " << principal << " from " << currency_names[currency1] << " -> " << currency_names[currency2] 
              << " @ rate " << rate << " => " << converted << "\n";
    deposit(currency2, converted);
}

double ArbitrageEngine::execute_cycle(const std::vector<std::vector<std::pair<double, int>>> &adj, const std::vector<int> &neg_cycle, int loop_count) {
    if (neg_cycle.size() < 2) return 0.0;

    double amount_copy = balances[base_currency];
    double amount = amount_copy * traded_per_cycle;

    for (int h = 0; h < loop_count; h++) {
        for (size_t i = 0; i + 1 < neg_cycle.size(); i++) {
            double rate = -1.0;
            for (const auto &edge : adj[neg_cycle[i]]) {
                if (edge.second == neg_cycle[i + 1]) {
                    rate = edge.first;
                    break;
                }
            }
            if (rate < 0) {
                std::cout << "Missing edge in cycle\n";
                return 0.0;
            }
            execute_exchange(neg_cycle[i], neg_cycle[i + 1], amount, rate);
            amount = balances[neg_cycle[i + 1]];
            if (amount <= 0) return 0.0;
        }
    }

    double profit = balances[base_currency] - amount_copy;
    std::cout << "Profit this cycle: " << profit << "\n";
    std::cout << "Net balance: " << balances[base_currency] << "\n";
    return profit;
}

double ArbitrageEngine::getBalance(int currency) {
    if (currency >= 0 && currency < static_cast<int>(balances.size()))
        return balances[currency];
    return 0.0;
}

void ArbitrageEngine::print_balances() {
    std::cout << "Balances: ";
    for (double b : balances) {
        std::cout << b << " ";
    }
    std::cout << "\nINR balance: " << balances[base_currency] << "\n";
}
