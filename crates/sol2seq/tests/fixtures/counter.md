sequenceDiagram
    participant CCaller as CounterCaller
    participant Counter as Counter
    
    CCaller->>Counter: increment()
    activate Counter
    Counter-->>CCaller: return count
    deactivate Counter
    
    CCaller->>Counter: getCount()
    activate Counter
    Counter-->>CCaller: return count
    deactivate Counter