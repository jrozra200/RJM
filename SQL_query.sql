SELECT DATE_FORMAT(orders.paid_at, '%Y') AS paid_year, SUM(orders.amount) AS annual_revenue, DATE_FORMAT(customers.created_at, '%Y') AS cohort_year
FROM Orders
LEFT JOIN Customers
ON orders.customer_id=customers.customer_id
GROUP BY cohort_year, paid_year;