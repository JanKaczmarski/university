#pragma once

module Demo {

    exception CounterNotFound {
        string name;
        string reason;
    };

    interface Counter {
        int getValue();
        void setValue(int val);
        int increment();
        void reset();
        string getName();
    };

};
