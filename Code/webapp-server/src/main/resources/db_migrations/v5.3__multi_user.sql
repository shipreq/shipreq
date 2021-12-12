CREATE SEQUENCE usr_group_seq START WITH 1;

CREATE TABLE usr_group (
  id          BIGINT PRIMARY KEY DEFAULT NEXTVAL('usr_group_seq'),
  name        VARCHAR NOT NULL,
  handle      VARCHAR NOT NULL UNIQUE,
  created_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at  TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TYPE usr_group_perm AS ENUM ('admin', 'member');

------------------------------------------------------------------------------------------------------------------------
-- usr_group_tree

CREATE TABLE usr_group_tree (
  from_id BIGINT NOT NULL REFERENCES usr_group,
  to_id   BIGINT NOT NULL REFERENCES usr_group,
  perm    usr_group_perm NOT NULL,
  UNIQUE(from_id, to_id, perm)
);

CREATE INDEX usr_group_tree_idx_to_from ON usr_group_tree(to_id, from_id);

CREATE FUNCTION usr_group_tree_prevent_cycle()
  RETURNS TRIGGER
  LANGUAGE plpgsql AS $$
BEGIN
  IF EXISTS (
    WITH RECURSIVE graph AS (
      SELECT t.from_id AS from_id, ARRAY[t.to_id, t.from_id] AS path, (from_id = to_id) AS cycle
        FROM usr_group_tree t
       WHERE t.from_id = NEW.from_id
      UNION ALL
      SELECT t.from_id, g.path || t.from_id, t.from_id = ANY(g.path)
        FROM graph g
        JOIN usr_group_tree t ON g.from_id = t.to_id
       WHERE NOT g.cycle
    )
    SELECT FROM graph WHERE cycle LIMIT 1
  )
  THEN
    RAISE EXCEPTION 'Cycle detected!';
  ELSE
    RETURN NEW;
  END IF;
END
$$;

CREATE TRIGGER usr_group_tree_prevent_cycle_trigger
AFTER INSERT OR UPDATE ON usr_group_tree
FOR EACH ROW EXECUTE PROCEDURE usr_group_tree_prevent_cycle();

------------------------------------------------------------------------------------------------------------------------
-- usr_group_usr

CREATE TABLE usr_group_usr (
  grp_id BIGINT NOT NULL REFERENCES usr_group,
  usr_id BIGINT NOT NULL REFERENCES usr,
  perm   usr_group_perm NOT NULL,
  UNIQUE(grp_id, usr_id, perm)
);

CREATE INDEX usr_group_usr_idx_usr_grp ON usr_group_usr(usr_id, grp_id);

------------------------------------------------------------------------------------------------------------------------
-- Functions

CREATE FUNCTION usr_group_tree_parent_roots(BIGINT[])
RETURNS TABLE(id usr_group.id%TYPE)
LANGUAGE SQL
STABLE -- function cannot modify the database, and that within a single table scan it will consistently return the same result for the same argument values
PARALLEL SAFE
AS $$
  WITH RECURSIVE x AS (
    SELECT id, ARRAY[]::BIGINT[] AS children FROM usr_group WHERE id = ANY($1)
    UNION ALL
    SELECT t.from_id, x.children || t.to_id
      FROM x, usr_group_tree t
     WHERE t.to_id = x.id
  )
  SELECT DISTINCT id
    FROM x
   WHERE id NOT IN (SELECT DISTINCT i FROM x, UNNEST(children) i)
$$;

CREATE FUNCTION usr_group_transitive_closure(BIGINT[])
RETURNS TABLE(
  from_id usr_group.id%TYPE,
  to_id   usr_group.id%TYPE,
  perm    usr_group_perm
)
LANGUAGE SQL
STABLE -- function cannot modify the database, and that within a single table scan it will consistently return the same result for the same argument values
PARALLEL SAFE
AS $$
  WITH RECURSIVE graph AS (
    SELECT * FROM usr_group_tree WHERE from_id = ANY($1)
    UNION ALL
    SELECT DISTINCT t.*
      FROM usr_group_tree t, graph g
     WHERE g.to_id = t.from_id
  )
  SELECT * FROM graph
$$;

CREATE FUNCTION usr_group_tree_universe(BIGINT[])
RETURNS TABLE(
  from_id usr_group.id%TYPE,
  to_id   usr_group.id%TYPE,
  perm    usr_group_perm
)
LANGUAGE SQL
STABLE -- function cannot modify the database, and that within a single table scan it will consistently return the same result for the same argument values
PARALLEL SAFE
AS $$
  SELECT * FROM usr_group_transitive_closure((
    SELECT ARRAY_AGG(id) FROM usr_group_tree_parent_roots($1)
  ))
$$;

CREATE FUNCTION usr_group_admin(BIGINT)
RETURNS TABLE(usr_id usr.id%TYPE)
LANGUAGE SQL
STABLE -- function cannot modify the database, and that within a single table scan it will consistently return the same result for the same argument values
PARALLEL SAFE
AS $$
  WITH RECURSIVE x AS (
    SELECT id, ARRAY[]::BIGINT[] AS children FROM usr_group WHERE id = $1
    UNION ALL
    SELECT t.from_id, x.children || t.to_id
      FROM x, usr_group_tree t
     WHERE t.to_id = x.id
  )
  SELECT usr_id FROM usr_group_usr
   WHERE grp_id IN (SELECT DISTINCT id FROM x)
     AND perm = 'admin'
$$;
