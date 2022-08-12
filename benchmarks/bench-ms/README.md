Scenarion: parallel merge sort ^[https://github.com/zakgof/akka-actr-benchmark]

A runner actor divides an array in two parts, then creates two new actors and sends them the halves. Then actor waits until both children reply with sorted arrays, merges them into a single sorted array and sends back to the parent actor. The test array had 2^20 random integers.

```
      Top-down step(fork)                         |
                                      [8, 5, 3, 2, 4, 6, 1, 7]
                                                  |
                                                  V
                                               Master
                                                  |
                                      [8, 5, 3, 2, 4, 6, 1, 7]
                                                  |
                                                  V
                                                Sorter
                                             /           \
                                    [8, 5, 3, 2]        [4, 6, 1, 7]              
                                        /                       \  
                                    Sorter                    Sorter     
                                   / \                         /     \  
                              [8,5]  [3,2]                 [4,6]     [1,7]
                               /        \                   /            \
                         Sorter         Sorter         Sorter           Sorter
                           /  \          / \             / \             /  \
                        [8]    [5]    [3]  [2]        [4]   [6]       [1]   [7] 
                        /        \    /       \       /       \       /        \
                      Sorter  Sorter Sorter  Sorter Sorter  Sorter  Sorter    Sorter 
                      
                      
                              
  Bottom-up step (join)               [1, 2, 3, 4, 5, 6, 7, 8]
                                                  ^
                                                  |
                                               Master
                                                  |
                                      [1, 2, 3, 4, 5, 6, 7, 8]
                                                  ^
                                                  |
                                                Sorter
                                             /           \
                                    [2, 3, 5, 8]        [1, 4, 6, 7]              
                                        /                       \  
                                    Sorter                    Sorter     
                                   / \                         /     \  
                              [5,8]  [2,3]                 [4,6]     [1,7]
                               /        \                   /            \
                         Sorter         Sorter         Sorter           Sorter
                           /  \          / \             / \             /  \
                        [8]    [5]    [3]  [2]        [4]   [6]       [1]   [7] 
                        /        \    /       \       /       \       /        \
                      Sorter  Sorter Sorter  Sorter Sorter  Sorter  Sorter    Sorter 
```