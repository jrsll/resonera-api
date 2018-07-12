curl -d '{"claim": {"name":"users", "value":"write", "userId":"1"}}' \
     -H "Content-Type: application/json" \
     -X DELETE \
     http://localhost:3000/api/admin/claims
