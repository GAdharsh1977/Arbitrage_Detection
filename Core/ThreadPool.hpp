#pragma once
#include <vector>
#include <thread>
#include <queue>
#include <deque>
#include <mutex>
#include <condition_variable>
#include <future>
#include <functional>
#include <atomic>

class ThreadPool {
public:
    std::vector<std::thread> threads;
    using task_t = std::function<void()>;
    std::queue<task_t, std::deque<task_t>> jobs;
    std::mutex queue_mutex;
    std::condition_variable cv;
    std::atomic<bool> stop{false};

    ThreadPool(size_t n) {
        for (size_t i = 0; i < n; i++) {
            threads.emplace_back([this]() {
                while (true) {
                    std::function<void()> task;
                    {
                        std::unique_lock<std::mutex> lock(queue_mutex);
                        cv.wait(lock, [this]() { return stop || !jobs.empty(); });
                        if (stop && jobs.empty()) return;
                        task = std::move(jobs.front());
                        jobs.pop();
                    }
                    task();
                }
            });
        }
    }

    template <class F>
    auto enqueue(F &&f) -> std::future<typename std::invoke_result<F>::type> {
        using return_type = typename std::invoke_result<F>::type;
        auto task = std::make_shared<std::packaged_task<return_type()>>(std::forward<F>(f));
        std::future<return_type> res = task->get_future();
        {
            std::lock_guard<std::mutex> lock(queue_mutex);
            jobs.emplace([task]() { (*task)(); });
        }
        cv.notify_one();
        return res;
    }

    ~ThreadPool() {
        {
            std::unique_lock<std::mutex> lock(queue_mutex);
            stop = true;
        }
        cv.notify_all();
        for (std::thread &worker : threads) {
            if (worker.joinable()) {
                worker.join();
            }
        }
    }

    int getThreadCount() const { return threads.size(); }

    void sleepAllThreads(int time_ms) {
        int n = getThreadCount();
        for (int i = 0; i < n; ++i) {
            enqueue([time_ms] {
                std::this_thread::sleep_for(std::chrono::milliseconds(time_ms));
            });
        }
    }
};
