#include <iostream>
#include <vector>
#include <climits>
#include <algorithm>
#include <cmath>
#include <unordered_set>
#include <chrono>
#include <thread>
#include <queue>
#include <deque>
#include <mutex>
#include <condition_variable>
#include <future>
using namespace std;

vector<string> currencies = {"INR", "USD", "JPY", "AUD"};
void logarithm(vector<vector<pair<double, int>>> &adjacency)
{
    for (auto &v : adjacency)
        for (auto &p : v)
            p.first = -log(p.first);
}

pair<vector<double>, vector<int>> bellmanFord(vector<vector<pair<double, int>>> &adjacency, int source)
{
    logarithm(adjacency);
    int n = adjacency.size();
    vector<double> distances(n, 1e9);
    vector<int> parent(n, -1);
    distances[source] = 0;

    for (int i = 0; i < n - 1; i++)
        for (int j = 0; j < n; j++)
            for (auto &k : adjacency[j])
                if (distances[j] != 1e9 && distances[k.second] > distances[j] + k.first)
                {
                    distances[k.second] = distances[j] + k.first;
                    parent[k.second] = j;
                }

    return make_pair(distances, parent);
}

vector<vector<int>> find_negative_cycles(vector<vector<pair<double, int>>> &adjacency, int source, double fee)
{
    // work on a copy so we don't destroy original rates.
    vector<vector<pair<double, int>>> adj_copy = adjacency;
    auto [distances, parent] = bellmanFord(adj_copy, source);

    // DEBUG
    int n = adjacency.size();
    int affected_count = 0;
    for (int j = 0; j < n; j++)
        for (auto &k : adj_copy[j])
            if (distances[j] != 1e9 && distances[k.second] > distances[j] + k.first)
                affected_count++;
    cout << "DEBUG affected nodes: " << affected_count << "\n";
    cout << "DEBUG cycle_nodes before backtrack: ";

    // find nodes affected by negative cycles
    unordered_set<int> affected;
    for (int j = 0; j < n; j++)
        for (auto &k : adj_copy[j])
            if (distances[j] != 1e9 && distances[k.second] > distances[j] + k.first)
                affected.insert(k.second);

    // backtrack v times to guarantee we land inside the cycle
    vector<int> cycle_nodes(affected.begin(), affected.end());
    for (int i = 0; i < n; i++)
        for (int &node : cycle_nodes)
            if (node != -1) node = parent[node];

    // DEBUG 2
    cout << "DEBUG cycle_nodes after backtrack: " << cycle_nodes.size() << " -> ";
    for (int node : cycle_nodes) cout << node << " ";
    cout << "\n";

    // remove duplicates
    sort(cycle_nodes.begin(), cycle_nodes.end());
    cycle_nodes.erase(unique(cycle_nodes.begin(), cycle_nodes.end()), cycle_nodes.end());
    cycle_nodes.erase(remove(cycle_nodes.begin(), cycle_nodes.end(), -1), cycle_nodes.end());

    // reconstruct cycles from parent pointers
    vector<vector<int>> all_cycles;
    for (int node : cycle_nodes)
    {
        vector<int> cycle;
        unordered_set<int> visited;
        int cur = node;

        while (visited.find(cur) == visited.end() && cur != -1)
        {
            cycle.push_back(cur);
            visited.insert(cur);
            cur = parent[cur];
        }
        if (!cycle.empty())
        {
            cycle.push_back(cycle.front());
            reverse(cycle.begin(), cycle.end());
        }

        // normalise — rotate so smallest element is first
        if (cycle.size() > 1)
        {
            cycle.pop_back();
            rotate(cycle.begin(), min_element(cycle.begin(), cycle.end()), cycle.end());
            cycle.push_back(cycle.front());
        }

        all_cycles.push_back(cycle);
    }

    // deduplicate
    sort(all_cycles.begin(), all_cycles.end());
    all_cycles.erase(unique(all_cycles.begin(), all_cycles.end()), all_cycles.end());

    // DEBUG 3
    cout << "DEBUG all_cycles count: " << all_cycles.size() << "\n";
    cout << "DEBUG cycles containing INR: ";
    for (auto &c : all_cycles)
        for (int node : c)
            if (node == 0) { cout << "found "; break; }
    cout << "\n";

    // filter — only keep INR cycles that are profitable after fees
    vector<vector<int>> result;
    for (auto &cycle : all_cycles)
    {
        if (cycle.size() < 2) continue;

        // must contain INR (node 0)
        bool has_inr = false;
        for (int node : cycle)
            if (node == 0) { has_inr = true; break; }
        if (!has_inr) continue;

        // simulate with original rates + fees
        double amount = 1.0;
        bool invalid = false;
        for (int i = 0; i + 1 < (int)cycle.size(); i++)
        {
            int u = cycle[i], v = cycle[i + 1];
            float rate = -1.0f;
            for (auto &edge : adjacency[u])
                if (edge.second == v) { rate = edge.first; break; }
            if (rate < 0) { invalid = true; break; }
            amount *= rate;
            amount *= (1.0 - fee);
            if (amount <= 0) { invalid = true; break; }
        }

         // DEBUG 4
        if (has_inr)
            cout << "DEBUG INR cycle profit simulation: " << amount << "\n";

        if (!invalid && amount > 1.005)
            result.push_back(cycle);
    }
    return result;
}

class threadpool
{
public:
    vector<thread> threads;
    using task_t = std::function<void()>;
    queue<task_t, deque<task_t>> jobs;
    mutex queue_mutex;
    condition_variable cv;
    atomic<bool> stop = false;

    threadpool(size_t n)
    {
        for (size_t i = 0; i < n; i++)
            threads.emplace_back([this]() {
                while (true) {
                    function<void()> task;
                    {
                        unique_lock<mutex> lock(queue_mutex);
                        cv.wait(lock, [this]() { return stop || !jobs.empty(); });
                        if (stop && jobs.empty()) return;
                        task = move(jobs.front());
                        jobs.pop();
                    }
                    task();
                }
            });
    }

    template <class F>
    auto enqueue(F &&f) -> future<typename std::invoke_result<F>::type>
    {
        using return_type = typename std::invoke_result<F>::type;
        auto task = make_shared<packaged_task<return_type()>>(forward<F>(f));
        future<return_type> res = task->get_future();
        {
            lock_guard<mutex> lock(queue_mutex);
            jobs.emplace([task]() { (*task)(); });
        }
        cv.notify_one();
        return res;
    }

    ~threadpool()
    {
        { unique_lock<mutex> lock(queue_mutex); stop = true; }
        cv.notify_all();
        for (auto &worker : threads) worker.join();
    }
};

class arbitrage_engine
{
private:
    vector<double> balances;
    int base_currency;
    float transaction_fee_pc;
    float traded_per_cycle;

public:
    arbitrage_engine(int size, double initial_capital, float fee, int base, float traded)
    {
        balances.resize(size, 0.0);
        balances[base] = initial_capital;
        transaction_fee_pc = fee;
        base_currency = base;
        traded_per_cycle = traded;
    }

    void deposit(int currency, double amount)
    {
        if (amount > 0 && currency >= 0 && currency < (int)balances.size())
            balances[currency] += amount;
    }

    double withdraw(int currency, double requested)
    {
        if (requested <= 0 || currency < 0 || currency >= (int)balances.size())
            return 0.0;
        double available = balances[currency];
        if (requested > available)
        {
            balances[currency] = 0.0;
            return available;
        }
        balances[currency] -= requested;
        return requested;
    }

    void execute_exchange(int c1, int c2, double req, double rate)
    {
        double principal = withdraw(c1, req);
        if (principal <= 0) return;
        double converted = principal * rate * (1.0 - transaction_fee_pc);
        converted = max(0.0, converted);
        cout << "Exchange " << principal << " " << currencies[c1]
             << " -> " << currencies[c2] << " @ " << rate
             << " => " << converted << "\n";
        deposit(c2, converted);
    }

    // cycle always starts and ends at INR
    double execute_cycle(vector<vector<pair<double, int>>> &adj, vector<int> &cycle, int loop_count)
    {
        if (cycle.size() < 2) return 0.0;

        double amount_copy = balances[base_currency];
        double amount = amount_copy * traded_per_cycle;

        for (int h = 0; h < loop_count; h++)
        {
            for (int i = 0; i + 1 < (int)cycle.size(); i++)
            {
                double rate = -1.0;
                for (auto &edge : adj[cycle[i]])
                    if (edge.second == cycle[i + 1]) { rate = edge.first; break; }
                if (rate < 0) { cout << "Missing edge\n"; return 0.0; }
                execute_exchange(cycle[i], cycle[i + 1], amount, rate);
                amount = balances[cycle[i + 1]];
                if (amount <= 0) return 0.0;
            }
        }

        double profit = balances[base_currency] - amount_copy;
        cout << "Profit: " << profit << "\n";
        cout << "Balance: " << balances[base_currency] << "\n";
        return profit;
    }

    double getBalance(int currency)
    {
        if (currency >= 0 && currency < (int)balances.size())
            return balances[currency];
        return 0.0;
    }

    void print_balances()
    {
        cout << "Balances: ";
        for (auto b : balances) cout << b << " ";
        cout << "\nINR balance: " << balances[base_currency] << "\n";
    }
};

// builds a simulated adjacency matrix with injected arbitrage opportunity
vector<vector<pair<double, int>>> buildSimulatedAdj()
{
    int n = currencies.size();
    vector<vector<pair<double, int>>> adj(n);

    // base rates vs INR
    vector<double> rates = {
        1.0,      // INR
        0.01182,  // USD
        1.7823,   // JPY
        0.01842,  // AUD
    };

    //calculation of rates not involving inr.
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
        {
            if (i == j) continue;
            adj[i].emplace_back(rates[j] / rates[i], j);
        }

    //inject profitable cycle: inr->usd->jpy->aud->inr.
    auto setRate = [&](int from, int to, double rate) {
        for (auto &e : adj[from])
            if (e.second == to) { e.first = rate; return; }
    };
    //boost in one direction to form a profitable cycle.
    setRate(0, 1, rates[1] / rates[0] * 1.5);  // INR->USD boosted
    setRate(1, 2, rates[4] / rates[1] * 1.5);  // USD->JPY boosted
    setRate(2, 3, rates[5] / rates[4] * 1.5);  // JPY->AUD boosted
    setRate(3, 0, rates[0] / rates[5] * 1.5);  // AUD->INR boosted
    return adj;
}

int main()
{
    srand(time(0));
    const double fee = 0.01;
    int iteration = 0;

    threadpool t(thread::hardware_concurrency() - 1);
    auto adj = buildSimulatedAdj();
    arbitrage_engine engine(adj.size(), 1.0, fee, 0, 0.5);

    while (true)
    {
        iteration++;
        cout << "iteration: " << iteration << "\n";

        adj = buildSimulatedAdj();

        auto cycles_future = t.enqueue([&]()
            { return find_negative_cycles(adj, 0, fee); });
        auto cycles = cycles_future.get();

        cout << "Profitable INR cycles found: " << cycles.size() << "\n";

        if (!cycles.empty())
        {
            //pick most profitable cycle.
            int best = 0;
            double best_profit = 0.0;
            for (int i = 0; i < (int)cycles.size(); i++)
            {
                double amount = 1.0;
                for (int j = 0; j + 1 < (int)cycles[i].size(); j++)
                {
                    for (auto &e : adj[cycles[i][j]])
                        if (e.second == cycles[i][j+1]) { amount *= e.first * (1.0 - fee); break; }
                }
                if (amount > best_profit) { best_profit = amount; best = i; }
            }

            auto &chosen = cycles[best];
            for (int i = 0; i < (int)chosen.size(); i++)
            {
                cout << currencies[chosen[i]];
                if (i < (int)chosen.size() - 1) cout << " -> ";
            }
            cout << "\n";

            if (engine.getBalance(0) > 0.01)
                engine.execute_cycle(adj, chosen, 1);
            else
                cout << "Insufficient INR balance\n";

            engine.print_balances();
        }
        else
        {
            cout << "No profitable cycles found\n";
        }

        this_thread::sleep_for(chrono::milliseconds(1000));
        cout << "\n";
    }
    return 0;
}