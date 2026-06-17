#pragma once
#include <vector>
#include <string>

class ArbitrageEngine {
private:
    std::vector<double> balances;
    int base_currency;
    double transaction_fee_pc;
    double traded_per_cycle;
    const std::vector<std::string>& currency_names;

public:
    ArbitrageEngine(int size, double initial_capital, double transaction_fee_pct, int base, double traded_in_cycle, const std::vector<std::string>& currencies);

    void deposit(int currency, double amount);
    double withdraw(int currency, double requested);
    void execute_exchange(int currency1, int currency2, double requested_amount, double rate);
    double execute_cycle(const std::vector<std::vector<std::pair<double, int>>> &adj, const std::vector<int> &neg_cycle, int loop_count);
    double getBalance(int currency);
    void print_balances();
};
