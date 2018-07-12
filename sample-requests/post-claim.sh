curl -d '{"claim": {"name":"users", "value":"write", "userId":"3"}}' \
     -H "Content-Type: application/json" \
     -X POST \
     http://localhost:3000/api/admin/claims
