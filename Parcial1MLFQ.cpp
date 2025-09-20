#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <deque>
#include <algorithm>
#include <numeric>
#include <queue>
#include <iomanip>
//David Bayona timana
// Represents a single process. All members are public.
class Process {
public:
    std::string tag;
    int initial_BT;
    int AT;
    int initial_Q; // Original queue from input file, for output purposes
    int Pr;

    // Dynamic state for simulation
    int remaining_time;
    int completion_time;
    int response_time;
    bool has_started;
    int current_q; // Current queue in the MLFQ simulation

    Process(std::string t, int bt, int at, int q, int pr)
        : tag(t), initial_BT(bt), AT(at), initial_Q(q), Pr(pr),
          remaining_time(bt), completion_time(-1), response_time(-1),
          has_started(false), current_q(1) {} // All processes start in Q1 for MLFQ
};

// Encapsulates the simulation logic. All members and methods are public.
class MLFQSimulator {
public:
    std::vector<Process> processes;
    std::vector<int> quantums;
    int lowest_priority_policy;

    // Constructor for the required scheme: RR(2), RR(3), RR(4), STCF
    MLFQSimulator() {
        quantums = {0, 2, 3, 4, 0}; 
        lowest_priority_policy = 1; // 1 = STCF
    }

    // Loads processes from a text file.
    bool loadProcesses(const std::string& filename) {
        std::ifstream file(filename);
        if (!file.is_open()) {
            std::cerr << "Error: Could not open input file " << filename << std::endl;
            return false;
        }

        std::string line;
        while (getline(file, line)) {
            line.erase(0, line.find_first_not_of(" \t\n\r"));
            line.erase(line.find_last_not_of(" \t\n\r") + 1);

            if (!(line.empty() || line[0] == '#')) {
                std::stringstream ss(line);
                std::string part;
                std::vector<std::string> parts;
                while (getline(ss, part, ';')) {
                    part.erase(0, part.find_first_not_of(" \t"));
                    part.erase(part.find_last_not_of(" \t") + 1);
                    parts.push_back(part);
                }

                if (parts.size() >= 5) {
                    processes.emplace_back(parts[0], stoi(parts[1]), stoi(parts[2]), stoi(parts[3]), stoi(parts[4]));
                }
            }
        }
        return true;
    }

    // Runs the main simulation loop.
    void run() {
        if (processes.empty()) return;

        std::vector<int> arrival_order(processes.size());
        std::iota(arrival_order.begin(), arrival_order.end(), 0);
        sort(arrival_order.begin(), arrival_order.end(), [&](int a, int b) {
            return processes[a].AT < processes[b].AT;
        });

        std::vector<std::deque<int>> ready_queues(5);
        int time = 0;
        int completed_processes = 0;
        int next_arrival_idx = 0;
        
        if(!arrival_order.empty()) {
            time = processes[arrival_order[0]].AT;
        }

        auto check_for_arrivals = [&](int current_time) {
            while (next_arrival_idx < processes.size() && processes[arrival_order[next_arrival_idx]].AT <= current_time) {
                int p_idx = arrival_order[next_arrival_idx];
                processes[p_idx].current_q = 1;
                ready_queues[1].push_back(p_idx);
                next_arrival_idx++;
            }
        };

        check_for_arrivals(time);

        while (completed_processes < processes.size()) {
            check_for_arrivals(time);

            int current_q_idx = 1;
            bool queue_found = false;
            while(current_q_idx <= 4 && !queue_found) {
                if(!ready_queues[current_q_idx].empty()) {
                    queue_found = true;
                } else {
                    current_q_idx++;
                }
            }
            if (!queue_found) {
                current_q_idx = 0;
            }

            if (current_q_idx == 0) {
                if (next_arrival_idx < processes.size()) {
                    time = processes[arrival_order[next_arrival_idx]].AT;
                    check_for_arrivals(time);
                }
            } else {
                if (current_q_idx <= 3) { // Round Robin for Q1-Q3
                    int p_idx = ready_queues[current_q_idx].front();
                    ready_queues[current_q_idx].pop_front();

                    if (!processes[p_idx].has_started) {
                        processes[p_idx].response_time = time - processes[p_idx].AT;
                        processes[p_idx].has_started = true;
                    }

                    int quantum = quantums[current_q_idx];
                    int run_time = std::min(quantum, processes[p_idx].remaining_time);

                    for(int i = 0; i < run_time; ++i) {
                        time++;
                        check_for_arrivals(time);
                    }
                    processes[p_idx].remaining_time -= run_time;

                    if (processes[p_idx].remaining_time == 0) {
                        processes[p_idx].completion_time = time;
                        completed_processes++;
                    } else {
                        int new_q = std::min(4, current_q_idx + 1);
                        processes[p_idx].current_q = new_q;
                        ready_queues[new_q].push_back(p_idx);
                    }
                } else { // STCF for Q4
                    auto it = std::min_element(ready_queues[4].begin(), ready_queues[4].end(), [&](int a, int b) {
                        return processes[a].remaining_time < processes[b].remaining_time;
                    });
                    int p_idx = *it;
                    ready_queues[4].erase(it);

                    if (!processes[p_idx].has_started) {
                        processes[p_idx].response_time = time - processes[p_idx].AT;
                        processes[p_idx].has_started = true;
                    }

                    time++;
                    processes[p_idx].remaining_time--;
                    check_for_arrivals(time);

                    if (processes[p_idx].remaining_time == 0) {
                        processes[p_idx].completion_time = time;
                        completed_processes++;
                    } else {
                        ready_queues[4].push_back(p_idx);
                    }
                }
            }
        }
    }

    // Writes the simulation results to a text file.
    void writeResults(const std::string& filename) {
        std::ofstream file(filename);
        if (!file.is_open()) {
            std::cerr << "Error: Could not open output file " << filename << std::endl;
            return;
        }

        file << "#etiqueta; BT; AT; Q; Pr; WT; CT; RT; TAT" << std::endl;

        double total_wt = 0, total_ct = 0, total_rt = 0, total_tat = 0;
        
        for (const auto& p : processes) {
            int tat = p.completion_time - p.AT;
            int wt = tat - p.initial_BT;

            total_wt += wt;
            total_ct += p.completion_time;
            total_rt += p.response_time;
            total_tat += tat;

            file << p.tag << ";" << p.initial_BT << ";" << p.AT << ";" 
                 << p.initial_Q << ";" << p.Pr << ";" << wt << ";" 
                 << p.completion_time << ";" << p.response_time << ";" << tat << std::endl;
        }
        
        int n = processes.size();
        if (n > 0) {
            file << std::fixed << std::setprecision(2);
            file << "WT=" << (total_wt / n) << "; "
                 << "CT=" << (total_ct / n) << "; "
                 << "RT=" << (total_rt / n) << "; "
                 << "TAT=" << (total_tat / n) << ";" << std::endl;
        }
    }
};

int main(int argc, char* argv[]) {
    if (argc != 3) {
        std::cout << "Usage: " << argv[0] << " <input_file.txt> <output_file.txt>" << std::endl;
        return 1;
    }

    std::string input_filename = argv[1];
    std::string output_filename = argv[2];

    MLFQSimulator simulator;

    if (!simulator.loadProcesses(input_filename)) {
        return 1;
    }

    simulator.run();
    simulator.writeResults(output_filename);

    std::cout << "Simulation complete. Results written to " << output_filename << std::endl;

    return 0;
}