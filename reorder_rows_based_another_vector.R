# https://stackoverflow.com/questions/27362718/reordering-rows-in-a-dataframe-according-to-the-order-of-rows-in-another-datafra

d2[ order(match(d2$variable, d1$Variable)), ]

train_nodes <- treatment_pre_interview_train_node_stats %>% select(terminal_node, p_weighted)
train_nodes

test_nodes <- treatment_pre_interview_test_node_stats %>% select(terminal_node, p_weighted)
test_nodes

match(test_nodes$terminal_node, train_nodes$terminal_node)
test_nodes[
        order(match(test_nodes$terminal_node, train_nodes$terminal_node)), ]


# try using a pure vector, not a dataframe
train_node_order <- train_nodes %>% pull(terminal_node)
train_node_order
test_nodes[order(match(test_nodes$terminal_node, train_node_order)), ]
