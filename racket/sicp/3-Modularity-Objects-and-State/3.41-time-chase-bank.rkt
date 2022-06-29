;; unserialized access to the bank balance can result in anomalous behavior

for example, user1 access balance while user2 deposite/withdraw balance
at the same time, would cause them to have different balance.