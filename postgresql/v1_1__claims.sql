create table claims (
  user_id bigserial references users(id),
  name varchar(10) not null,
  value varchar(15) not null,
  PRIMARY KEY(user_id, name, value)
);
