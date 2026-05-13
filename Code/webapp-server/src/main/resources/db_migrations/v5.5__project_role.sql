ALTER TYPE project_perm RENAME TO project_role;
ALTER TABLE project_access RENAME COLUMN perm TO role;
