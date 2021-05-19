This folder contains real-world temporal automatons. 
Example MutualExclusion.txt is based on the ficher mutual exclusion algorithm.     

ficher mutual exclusion algorithm
    
    loop
        wait until lock = 0; % sleeping state
        wait for a delay <= delta1; % waiting state
        set lock to process id;
        wait for a delay >= delta2; % trying state
        if lock = process id
            critical section; % critical state
            set lock to 0;
        end
    end

Example Gate.txt, Controller.txt and Train.txt are a simple synchronous composition of tree timed automata.
Models a railway-crossing system that consists of a train, a gate an a controller.