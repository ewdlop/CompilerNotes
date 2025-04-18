using System;
using System.Collections.Generic;
using System.Linq;

namespace CYKParser
{
    public class Grammar
    {
        // Dictionary to store productions: Key = Non-terminal, Value = List of possible productions
        private Dictionary<char, List<string>> _productions;
        private char _startSymbol;

        public Grammar(char startSymbol)
        {
            _productions = new Dictionary<char, List<string>>();
            _startSymbol = startSymbol;
        }

        // Add a production rule to the grammar
        public void AddProduction(char nonTerminal, string production)
        {
            if (!_productions.ContainsKey(nonTerminal))
            {
                _productions[nonTerminal] = new List<string>();
            }
            _productions[nonTerminal].Add(production);
        }

        // Get all non-terminals that can derive the given string
        private HashSet<char> GetNonTerminals(string str)
        {
            HashSet<char> result = new HashSet<char>();
            
            foreach (var entry in _productions)
            {
                if (entry.Value.Contains(str))
                {
                    result.Add(entry.Key);
                }
            }
            
            return result;
        }

        // CYK Algorithm implementation
        public bool Parse(string input)
        {
            int n = input.Length;
            
            // Initialize the DP table
            // table[i,j] contains the set of non-terminals that can derive the substring input[i..i+j-1]
            HashSet<char>[,] table = new HashSet<char>[n + 1, n + 1];
            
            for (int i = 0; i <= n; i++)
            {
                for (int j = 0; j <= n; j++)
                {
                    table[i, j] = new HashSet<char>();
                }
            }
            
            // Fill in the table for substrings of length 1
            for (int i = 0; i < n; i++)
            {
                string terminal = input[i].ToString();
                HashSet<char> nonTerminals = GetNonTerminals(terminal);
                
                foreach (char nt in nonTerminals)
                {
                    table[i, 1].Add(nt);
                }
            }
            
            // Fill in the table for substrings of length 2 to n
            for (int j = 2; j <= n; j++)
            {
                for (int i = 0; i <= n - j; i++)
                {
                    for (int k = 1; k < j; k++)
                    {
                        // Check all possible splits of the substring
                        foreach (char B in table[i, k])
                        {
                            foreach (char C in table[i + k, j - k])
                            {
                                // For each non-terminal that can derive BC
                                HashSet<char> nonTerminals = GetNonTerminals(B.ToString() + C.ToString());
                                
                                foreach (char A in nonTerminals)
                                {
                                    table[i, j].Add(A);
                                }
                            }
                        }
                    }
                }
            }
            
            // The input string is in the language if the start symbol can derive the entire string
            return table[0, n].Contains(_startSymbol);
        }

        // Print the grammar
        public void PrintGrammar()
        {
            Console.WriteLine("Grammar:");
            foreach (var entry in _productions)
            {
                Console.Write($"{entry.Key} -> ");
                Console.WriteLine(string.Join(" | ", entry.Value));
            }
        }
    }
}
