Struct = {
  % It's said that atoms don't apply to garbage collection. Think about changing this pomiar to String
    pomiar,
    'London',
    % YYYY, MM, DD, hh, mm, ss
    {date(), time()},
    [
     % RodzajCzytnika, wartosc
     {'PM10', 123},
     {'PM2.5', 50}
    ]
 }.

P1 = {
          pomiar,
          'Delhi',
          {date(), time()},
          [
           {'PM10', 123},
           {'PM2.5', 50}
          ]
       }.

P2  = {
          pomiar,
          'Warsaw',
          {date(), time()},
          [
           {'PM10', 23}
          ]
       }.

P3  = {
          pomiar,
          'Moskow',
          {date(), time()},
          [
           {'PM10', 23}
          ]
       }.

ListaPomiarow = [P1, P2, P3].

 P4  = {
              pomiar,
              'Seattle',
              % YYYY, MM, DD, hh, mm, ss
              {date(), time()},
              [
               {'PM2.5', 2000}
              ]
           }.

NowaListaPomiarow = [P4 | ListaPomiarow].
NazwaP1 = element(2, P1).
