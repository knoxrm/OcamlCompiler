# CSCI 6120 Compilers - Syllabus
# Learning Outcomes
I aim to gain a solid understanding of compilers and walk away with strong foundation in compiler design and implementation. The way I plan to do it is by implementing a project as my final project for the class. 

1.  **Understanding Compiler Structure**: - - Develop a strong understanding of the major components of compilers, including lexical analysis, syntax analysis, semantic analysis, optimization, and code generation.
    
2.  **Programming Language Theory**: Apply programming language concepts and theory in the process of writing a compiler, enhancing both theoretical knowledge and practical skills.
    
3.  **Compiler Optimization Techniques**: Gain in-depth knowledge of compiler optimization techniques and loop transformations, inspired by collaborative research presentations with my classmates, Jason and Ian, in the "Intro to Research" course.
    
4.  **Practical Compiler Construction**: Undertake a hands-on project, such as developing a simple interpreter for a subset of Lua or a compiler for a subset of C, to solidify practical compiler construction skills.
   
   These goals will be expanded more at week 3 or 4. after more time has been spent on understanding of the course material. 


# Timeline

<!-- Tweak the following table as needed to enter your goal, timelines, deliverables, … -->

The program will start on May 20 and finish on July 29th, 2024.


| Week         | Event                                               | Due                                                                                                                                                       |
| ------------ | --------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1 (May 20)   | Introduction to Compiler Design                     | -                                                                                                                                                         |
| 2 (May 27)   | Memorial Day (Out of town due to summer school)<br> | -                                                                                                                                                         |
| 3 (June 3)   | Lexical Analysis and Syntax Analysis                | Reading assignment #1: [Correctness of a Compiler for Arithmetic Expressions](http://jmc.stanford.edu/articles/mcpain.html)                               |
| 4 (June 10)  | Semantic Analysis                                   |                                                                                                                                                           |
| 5 (June 17)  | Juneteenth Independence Day                         | Reading assignment #2: [Finding and Understanding Bugs in C Compilers](https://users.cs.utah.edu/~regehr/papers/pldi11-preprint.pdf) and Project Proposal |
| 6 (June 24)  | Intermediate Representation (IR)                    | -                                                                                                                                                         |
| 7 (July 1)   | Independence Day Observance                         | -                                                                                                                                                         |
| 8 (July 8)   | Basic Optimizations                                 | -                                                                                                                                                         |
| 9 (July 15)  | Loop Optimizations                                  | Reading assignment #3: [Deniable Backdoors Using Compiler Bugs](https://www.alchemistowl.org/pocorgtfo/pocorgtfo08.pdf#page=7)                            |
| 10 (July 22) | Class ends                                          | -                                                                                                                                                         |
| 11 (July 29) | Project Implementation and Presentation             | -                                                                                                                                                         |
<!--

Week | Event | Due
--- | ----------- | ---------------------
1 (May 20) | Introduction to Compiler Design | Assignment: Simple lexical analyzer using regular expressions
2 (May 27) | Memorial Day | Assignment: Implement a basic lexer and parser for a simple language
3 (June 3) | Lexical Analysis and Syntax Analysis | Reading assignment #1: [Correctness of a Compiler for Arithmetic Expressions](http://jmc.stanford.edu/articles/mcpain.html)
4 (June 10) | Semantic Analysis | Assignment: Extend the parser to include semantic analysis
5 (June 17) | Juneteenth Independence Day | Reading assignment #2: [Finding and Understanding Bugs in C Compilers](https://users.cs.utah.edu/~regehr/papers/pldi11-preprint.pdf)
6 (June 24) | Intermediate Representation (IR) | Assignment: Implement an IR for the simple language
7 (July 1) | Independence Day Observance | Assignment: Implement constant folding, dead code elimination, CSE, and strength reduction in the IR
8 (July 8) | Loop Optimizations | Assignment: Implement loop unrolling and invariant code motion in the IR
9 (July 15) | Intermediate-Level Optimizations | Reading assignment #3: [Deniable Backdoors Using Compiler Bugs](https://www.alchemistowl.org/pocorgtfo/pocorgtfo08.pdf#page=7)
10 (July 22) | Advanced Optimizations and Backend Optimizations | Assignment: Implement a simple interprocedural optimization and basic instruction scheduling
11 (July 29) | Project Implementation and Presentation | Final project report and code submission


| Week | Date(s)     | Minimum                                                                                   | Bonus                                       | Extra Reading                                                                                                                        | Due                                                                                                                            |
| :--: | :---------- | :---------------------------------------------------------------------------------------- | :------------------------------------------ | :----------------------------------------------------------------------------------------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------- |
|  1   | May 20, 22  | Lesson 1: Welcome & Overview, Performance and Measurement (Adrian)                        |                                             |                                                                                                                                      |                                                                                                                                |
|      | May 24      | Lesson 2: Representing Programs                                                           | Intermediate Representations (IRs)          |                                                                                                                                      |                                                                                                                                |
|  2   | May 27, 29  | Lesson 3: Local Analysis & Optimization                                                   |                                             |                                                                                                                                      |                                                                                                                                |
|  3   | June 3, 5   | Lesson 4: Data Flow                                                                       |                                             | Reading assignment #1: [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf)          | [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf)                           |
|      | June 7      | Lesson 5: Global Analysis & SSA                                                           | Type Systems (Overview)                     |                                                                                                                                      | Project Proposal Proposal                                                                                                      |
|  4   | June 10, 12 | Lesson 5: Global Analysis & SSA (cont.)                                                   |                                             |                                                                                                                                      |                                                                                                                                |
|      | June 14     | Register Allocation, Instruction Scheduling                                               |                                             |                                                                                                                                      | Project Proposal (final)                                                                                                       |
|  5   | June 17, 19 | Lesson 7: Loop Optimization, Lesson 8: Interprocedural Analysis                           | Polymorphism & Type Inference               | Reading assignment #2: [Finding and Understanding Bugs in C Compilers](https://users.cs.utah.edu/~regehr/papers/pldi11-preprint.pdf) | Reading assignment #3: [Deniable Backdoors Using Compiler Bugs](https://www.alchemistowl.org/pocorgtfo/pocorgtfo08.pdf#page=7) |
|      | June 21, 24 | Lesson 9: Alias Analysis                                                                  |                                             |                                                                                                                                      |                                                                                                                                |
|  6   | June 26, 28 | Lesson 10: Memory Management, Garbage Collection (GC) & Reference Counting (Mark Moeller) | Modern GC (Patrick LaFontaine)              |                                                                                                                                      |                                                                                                                                |
|      | July 1      |                                                                                           |                                             |                                                                                                                                      |                                                                                                                                |
|  7   | July 3      | Lesson 11: Dynamic Compilers                                                              |                                             |                                                                                                                                      |                                                                                                                                |
|      | July 8      | Superoptimization (Socrates Wong)                                                         |                                             |                                                                                                                                      |                                                                                                                                |
|  8   | July 10, 12 | Lesson 13: Concurrency & Parallelism                                                      | Formal Semantics, Type Safety and Soundness |                                                                                                                                      |                                                                                                                                |
|      | July 15     | Domain-Specific Languages (DSLs), Compiler Construction Tools                             | Vectorization (Ankush Rayabhari)            | Reading assignment #3: [Deniable Backdoors Using Compiler Bugs](https://www.alchemistowl.org/pocorgtfo/pocorgtfo08.pdf#page=7)       |                                                                                                                                |
|      | July 17     | Safe Parallelism (Hanchen Jin & Xinwen Wang), Program Analysis Techniques                 | Interactive Verification (Priya Srikumar)   |                                                                                                                                      |                                                                                                                                |
|      | July 22     | **Final Project Due** (no class)                                                          |                                             |                                                                                                                                      |                                                                                                                                |
|      |             |                                                                                           |                                             |                                                                                                                                      |                                                                                                                                |
-->
<!-- Once you are done with your timeline, please go back to your learning outcomes (research questions / goals), and wonder: are you giving you enough time to complete them all? Did new learning outcomes emerge from your timeline? If your mapping from weeks to learning outcomes, or from learning outcomes to weeks is partial, then something is wrong.-->

# Tools

<!-- List the tools & services you will be using. Please, prefer cross-OS, open-source & free tools as much as possible, and prefer if possible services that are free of charge. -->
- Bril
- Yacc
- Neovim
- Fish shell
- I use Arch btw

# Resources
- [Compiler's Playlist from Stanford](https://www.youtube.com/playlist?list=PLTsf9UeqkRebOYdw4uqSN0ugRShSmHrzH)
- [Compilers: Principles, Techniques, and Tools](https://www.amazon.com/Compilers-Principles-Techniques-Alfred-Aho/dp/0201100886)

<!--
List the resources you plan on using, ideally with precise bibliographical references and / or links.
Be specific: don't go on listing all the textbooks ever written on compilers, but refer precisely to e.g., Chapters or Sections of various material. Ideally, you could even tie those references back to your learning outcomes and (transitively) to your timeline.
-->