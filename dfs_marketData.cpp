#include <iostream>
#include <vector>
#include <stack>
#include <algorithm>
#include <unordered_set>
#include <chrono>
#include <thread>
#include <mutex>
#include <queue>
#include <functional>
#include <condition_variable>
#include <future>
#include "json.hpp"
#include <curl/curl.h>
#include <string>
using namespace std;
using json = nlohmann::json;
string cacert = "Your cacert's path";
vector<string> currencies = {"INR", "USD", "EUR", "GBP", "JPY", "AUD", "CAD", "CHF", "TRY", "ZAR", "CNY", "HKD", "SGD", "NZD", "MXN", "KRW", "BRL", "RUB", "SEK", "NOK", "PLN", "DKK", "MYR", "THB", "ILS"};
vector<string> targets = {"AUD", "USD", "EUR", "GBP", "JPY", "CAD", "CHF", "TRY", "ZAR", "CNY", "HKD", "SGD", "NZD", "MXN", "KRW", "BRL", "RUB", "SEK", "NOK", "PLN", "DKK", "MYR", "THB", "ILS"};

void logarithm(vector<vector<pair<float, int>>> &adjacency)
{
    for (vector<pair<float, int>> &v : adjacency)
    {
        for (pair<float, int> &p : v)
        {
            p.first = -log(p.first);
        }
    }
}

vector<bool> eliminate_unnecesscary(vector<vector<pair<float, int>>> &adjacency, int source)
{
    int N = adjacency.size();
    vector<bool> reachable(N, false);
    queue<int> q;

    reachable[source] = true;
    q.push(source);

    while (!q.empty())
    {
        int u = q.front();
        q.pop();

        for (auto &edge : adjacency[u])
        {
            int v = edge.second;
            if (!reachable[v])
            {
                reachable[v] = true;
                q.push(v);
            }
        }
    }
    return reachable;
}

vector<vector<pair<float, int>>> remove_edges(vector<vector<pair<float, int>>> &adjacency, int source)
{
    vector<bool> reachable = eliminate_unnecesscary(adjacency, source);
    for (int i = 0; i < adjacency.size(); i++)
    {
        if (reachable[i])
        {
            auto &edges = adjacency[i];
            edges.erase(remove_if(edges.begin(), edges.end(),
                                  [&](const pair<float, int> &p)
                                  {
                                      return (p.second >= adjacency.size() ||
                                              (!reachable[p.second] && p.second != source)); // keep edges to source
                                  }),
                        edges.end());
        }
    }
    return adjacency;
}

// Find all cycles in the graph starting from source
// signature change
vector<vector<int>> dfs(vector<vector<pair<float, int>>> &adjacency, int max_len, int source)
{
    adjacency = remove_edges(adjacency, source); // prune unreachable nodes
    vector<vector<int>> all_cycles;
    stack<int> dfs_stack;
    vector<int> path;
    unordered_set<int> inPath;

    if (source < 0 || source >= static_cast<int>(adjacency.size()))
        return all_cycles;

    dfs_stack.push(source); // start from source

    while (!dfs_stack.empty())
    {
        int node = dfs_stack.top();
        dfs_stack.pop();

        if (node == -1)
        { // backtrack marker
            if (!path.empty())
            {
                int last = path.back();
                path.pop_back();
                inPath.erase(last);
            }
            continue;
        }

        if (node < 0 || node >= static_cast<int>(adjacency.size()))
            continue;

        if (inPath.count(node))
        {
            // cycle detected: from first occurrence to current node
            auto it = find(path.begin(), path.end(), node);
            if (it != path.end())
            {
                vector<int> cycle(it, path.end());
                cycle.push_back(node);
                if (cycle.size() >= 2)
                { // only store cycles with >1 node.
                    all_cycles.push_back(cycle);
                }
            }
            continue;
        }

        path.push_back(node);
        inPath.insert(node);
        dfs_stack.push(-1); // backtrack marker

        for (auto it = adjacency[node].rbegin(); it != adjacency[node].rend(); ++it)
        {
            int neighbor = it->second;
            if (neighbor >= static_cast<int>(adjacency.size()) || neighbor < 0)
                continue;
            if (path.size() >= max_len)
                continue;

            if (inPath.count(neighbor))
            {
                auto it2 = find(path.begin(), path.end(), neighbor);
                if (it2 != path.end())
                {
                    vector<int> cycle(it2, path.end());
                    cycle.push_back(neighbor); // close the cycle.
                    all_cycles.push_back(cycle);
                }
            }
            else
            {
                dfs_stack.push(neighbor);
            }
        }
    }
    return all_cycles;
}
void normalise_cycles(vector<vector<pair<float, int>>> &adjacency, vector<vector<int>> &cycles, vector<float> &weights)
{
    for (auto &c : cycles)
    {
        if (c.empty())
            continue;
        c.pop_back();
        rotate(c.begin(), min_element(c.begin(), c.end()), c.end());
    }

    unordered_set<size_t> unique_hashes;
    vector<vector<int>> deduped;
    for (auto &cycle : cycles)
    {
        size_t h = 0;
        for (int x : cycle)
            h = h * 31 + static_cast<size_t>(x);
        if (unique_hashes.insert(h).second)
            deduped.push_back(cycle);
    }
    cycles = std::move(deduped);

    // completing the cycle (pushing the first element of the cycle.)
    for (int i = 0; i < static_cast<int>(cycles.size()); ++i)
    {
        if (!cycles[i].empty())
            cycles[i].push_back(cycles[i].front());
    }
    weights.assign(cycles.size(), 0.0f);
}

pair<vector<vector<int>>, int> filter_negative_cycles(
    vector<vector<int>> &cycles,
    vector<vector<pair<float, int>>> &adjacency,
    vector<float> &weights,
    double transaction_fee_pc = 0.01)
{
    vector<vector<int>> negative_cycles;
    vector<double> neg_profits;

    for (int i = 0; i < static_cast<int>(cycles.size()); i++)
    {
        if (cycles[i].size() < 2)
            continue;

        // check if INR is in this cycle
        bool has_inr = false;
        for (int node : cycles[i])
            if (node == 0)
            {
                has_inr = true;
                break;
            }
        if (!has_inr)
            continue;

        // simulate the cycle with fees.
        double amount = 1.0;
        bool invalid = false;

        for (int j = 0; j + 1 < static_cast<int>(cycles[i].size()); j++)
        {
            int u = cycles[i][j];
            int v = cycles[i][j + 1];

            if (u < 0 || u >= static_cast<int>(adjacency.size()) ||
                v < 0 || v >= static_cast<int>(adjacency.size()))
            {
                invalid = true;
                break;
            }

            float rate = -1.0f;
            for (auto &edge : adjacency[u])
            {
                if (edge.second == v)
                {
                    rate = edge.first;
                    break;
                }
            }

            if (rate < 0)
            {
                invalid = true;
                break;
            }

            amount *= rate;
            amount *= (1.0 - transaction_fee_pc);
            if (amount <= 0)
            {
                invalid = true;
                break;
            }
        }

        if (invalid)
            continue;

        // require at least 0.5% profit after all fees(for substantive change in output.)
        if (amount > 1.005)
        {
            negative_cycles.push_back(cycles[i]);
            neg_profits.push_back(amount);
        }
    }

    int best_idx = -1;
    if (!neg_profits.empty())
    {
        best_idx = 0;
        for (int k = 1; k < static_cast<int>(neg_profits.size()); ++k)
            if (neg_profits[k] > neg_profits[best_idx])
                best_idx = k;
    }

    return make_pair(negative_cycles, best_idx);
}

void print(vector<vector<int>> &v)
{
    for (auto &cycle : v)
    {
        for (auto x : cycle)
            std::cout << x << " ";
        std::cout << "\n";
    }
}
void print(vector<int> &v)
{
    for (auto &x : v)
    {
        std::cout << x << " ";
    }
    std::cout << "\n";
}

class arbitrage_engine
{
private:
    vector<double> balances;
    int base_currency; // index of base currency
    float transaction_fee_pc;
    float traded_per_cycle;

public:
    // construct a bank account.
    arbitrage_engine(int size, double initial_capital, float transaction_fee_pct, int base, float traded_in_cycle)
    {
        balances.resize(size, 0.0f);
        balances[base] = initial_capital;
        transaction_fee_pc = transaction_fee_pct;
        base_currency = base;
        traded_per_cycle = traded_in_cycle;
    }
    void deposit(int currency, double amount)
    {
        if (amount > 0 && currency >= 0 && currency < balances.size())
        {
            balances[currency] += amount;
        }
        else if (currency < 0 || currency >= balances.size())
        {
            std::cout << "Invalid index for deposit: " << currency << std::endl;
        }
    }
    double withdraw(int currency, double requested)
    {
        if (requested <= 0 || currency < 0 || currency >= static_cast<int>(balances.size()))
        {
            std::cout << "Invalid amount or index: " << requested << endl;
            return 0.0;
        }
        double available = balances[currency];
        if (requested > available)
        {
            std::cout << "Insufficient funds for " << currency << ": withdrawing all $" << available << endl;
            balances[currency] = 0.0;
            return available;
        }
        balances[currency] -= requested;
        return requested;
    }
    void execute_exchange(int currency1, int currency2, double requested_amount, float rate)
    {
        double principal = withdraw(currency1, requested_amount);
        if (principal <= 0)
            return;

        double converted = principal * rate;
        converted -= converted * transaction_fee_pc;
        converted = max(0.0, converted); // prevent negative values.
        std::cout << "Exchange " << principal << " from " << currencies[currency1] << "->" << currencies[currency2] << " at rate " << rate
                  << " => " << converted << "\n";
        deposit(currency2, converted);
    }
    // pass path by value. not reference because we are reversing it, and we don't want the -ve cycle to be reversed randomly.
    double execute_cycle(vector<vector<pair<float, int>>> &adj, vector<int> &neg_cycle, int loop_count)
    {
        if (neg_cycle.size() < 2)
            return 0.0;

        // cycle always starts at INR.
        double amount = balances[base_currency];
        double amount_copy = amount;
        amount *= traded_per_cycle;
        float rate;

        for (int h = 0; h < loop_count; h++)
        {
            for (int i = 0; i < (int)neg_cycle.size() - 1; i++)
            {
                rate = -1.0f;
                for (auto &edge : adj[neg_cycle[i]])
                    if (edge.second == neg_cycle[i + 1])
                    {
                        rate = edge.first;
                        break;
                    }
                if (rate < 0)
                {
                    std::cout << "Missing edge in cycle\n";
                    return 0.0;
                }
                execute_exchange(neg_cycle[i], neg_cycle[i + 1], amount, rate);
                amount = balances[neg_cycle[i + 1]];
                if (amount <= 0)
                    return 0.0;
            }
        }

        double profit = balances[base_currency] - amount_copy;
        std::cout << "Profit this cycle: " << profit << "\n";
        std::cout << "Net balance: " << balances[base_currency] << "\n";
        return profit;
    }
    double getBalance(int currency)
    {
        if (currency >= 0 && currency < balances.size())
            return balances[currency];
        return 0.0;
    }
};

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
        {
            threads.emplace_back([this]()
                                 {
                while(true){
                    function<void()> task;
                    { 
                        unique_lock<mutex> lock(queue_mutex);
                        cv.wait(lock, [this](){return stop || !jobs.empty();});
                        if(stop and jobs.empty()) return;
                        task = move(jobs.front());
                        jobs.pop();
                    }
                    task();
                } });
        }
    }
    template <class F>
    auto enqueue(F &&f) -> future<typename std::invoke_result<F>::type>
    {
        using return_type = typename std::invoke_result<F>::type;
        auto task = std::make_shared<std::packaged_task<return_type()>>(std::forward<F>(f));
        std::future<return_type> res = task->get_future();
        {
            lock_guard<mutex> lock(queue_mutex);
            jobs.emplace([task]()
                         { (*task)(); });
        }
        cv.notify_one();
        return res;
    }
    ~threadpool()
    {
        {
            std::unique_lock<std::mutex> lock(queue_mutex);
            stop = true;
        }
        cv.notify_all();
        for (std::thread &worker : threads)
            worker.join();
    }
    int getThreadCount() const { return threads.size(); }

    void sleepAllThreads(int time) // time is given in milliseconds.
    {
        int n = getThreadCount();
        for (int i = 0; i < n; ++i)
        {
            enqueue([time]
                    { std::this_thread::sleep_for(std::chrono::milliseconds(time)); });
        }
    }
};

size_t writeCallBack(void *contents, size_t size, size_t nmemb, string *userp)
{
    size_t realSize = size * nmemb;
    userp->append((char *)contents, realSize);
    return realSize;
}

string fetchRatesWithAPI(vector<string> &targets)
{
    CURL *curl = curl_easy_init();
    if (!curl)
    {
        std::cout << "Curl not initialised." << std::endl;
        return "";
    }
    string Additions;
    for (auto &currency : targets)
    {
        if (currency != "INR")
        {
            if (!Additions.empty())
                Additions += ",";
            Additions += currency;
        }
    }
    if (Additions.empty())
    {
        std::cout << "No targets given." << std::endl;
        return "";
    }
    string url = "https://api.frankfurter.app/latest?from=INR&to=" + Additions;

    string readBuffer;
    curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 10L);
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallBack);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);

    curl_easy_setopt(curl, CURLOPT_CAINFO, cacert);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 2L);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 2L);

    CURLcode res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK)
    {
        std::cerr << "Fetch failed: " << curl_easy_strerror(res) << " (Error code: " << res << ")" << std::endl;
        return "";
    }
    if (readBuffer.empty())
    {
        std::cout << "Void response." << std::endl;
        return "";
    }
    if (readBuffer.find("\"base\":\"INR\"") == string::npos)
    {
        std::cerr << "API error." << std::endl;
        return "";
    }
    return readBuffer;
}

vector<vector<pair<float, int>>> buildAdj(string &json_str, vector<string> &currencies)
{
    int n = currencies.size();
    vector<vector<pair<float, int>>> adj(n);

    try
    {
        auto parsed = nlohmann::json::parse(json_str);
        if (!parsed.contains("base") || parsed["base"] != "INR")
        {
            std::cerr << "INR is not the base." << std::endl;
            return adj;
        }

        auto rates_obj = parsed["rates"];
        if (rates_obj.is_null())
        {
            std::cerr << "No rates." << std::endl;
            return adj;
        }

        vector<float> rates(n);
        rates[0] = 1.0f;

        for (int i = 1; i < n; i++)
        {
            string cur = currencies[i];
            if (rates_obj.contains(cur))
                rates[i] = rates_obj[cur].get<float>();
            else
                rates[i] = 0.01f;
        }

        for (int i = 0; i < n; i++)
        {
            for (int j = 0; j < n; j++)
            {
                if (i == j)
                    continue;
                float noise = 1.0f + ((rand() % 600) - 300) * 0.0001f; // ±3% per edge
                float w = static_cast<float>(rates[j] / rates[i]) * noise;
                adj[i].emplace_back(make_pair(w, j));
            }
        }
        return adj;
    }
    catch (std::exception &e)
    {
        std::cerr << "Parse error: " << e.what() << std::endl;
        return adj;
    }
}

int main()
{
    srand(time(0));
    int iteration = 0;
    const double fee = 0.01;
    threadpool t(thread::hardware_concurrency() - 1);

    // first fetch to size the engine
    string json_str = fetchRatesWithAPI(targets);
    auto adj = buildAdj(json_str, currencies);
    arbitrage_engine engine(adj.size(), 1.0, fee, 0, 0.5);

    while (true)
    {
        iteration++;
        std::cout << "iteration: " << iteration << "\n";

        // re-fetch each iteration — no redeclaration, just reassign
        json_str = fetchRatesWithAPI(targets);
        adj = buildAdj(json_str, currencies);
        if (adj[0].empty())
        {
            std::cerr << "Adj empty; check API." << std::endl;
            return 1;
        }

        auto cycles_future = t.enqueue([&]()
                                       { return dfs(adj, 5, 0); });
        auto cycles = cycles_future.get();

        int j = adj.size();
        vector<float> weights(j, 0);
        t.enqueue([&]()
                  { normalise_cycles(adj, cycles, weights); })
            .get();

        auto neg_cycles_future = t.enqueue([&]()
                                           { return filter_negative_cycles(cycles, adj, weights, fee); });
        auto neg_cycles = neg_cycles_future.get();
        vector<vector<int>> negative_cycles = neg_cycles.first;
        int best_index = neg_cycles.second;

        std::cout << "Profitable INR cycles found: " << negative_cycles.size() << "\n";

        if (!negative_cycles.empty() && best_index >= 0)
        {
            vector<int> chosen_cycle = negative_cycles[best_index];

            for (int i = 0; i < (int)chosen_cycle.size(); i++)
            {
                std::cout << currencies[chosen_cycle[i]];
                if (i < (int)chosen_cycle.size() - 1)
                    std::cout << " -> ";
            }
            std::cout << "\n";

            if (engine.getBalance(0) > 0.01)
            {
                engine.execute_cycle(adj, chosen_cycle, 1);
            }
            else
            {
                std::cout << "Insufficient INR balance\n";
            }
        }
        else
        {
            std::cout << "No profitable cycles found this iteration\n";
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(1000));
        std::cout << "\n";
    }
    return 0;
}