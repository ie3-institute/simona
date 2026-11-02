CREATE TABLE public.load_profiles
(
    load_profile VARCHAR(11) PRIMARY KEY,
    quarter_hour integer NOT NULL
)
    WITHOUT OIDS
    TABLESPACE pg_default;

INSERT INTO
    public.load_profiles (load_profile, quarter_hour)
VALUES
('h0', 0),
('g2', 0);
