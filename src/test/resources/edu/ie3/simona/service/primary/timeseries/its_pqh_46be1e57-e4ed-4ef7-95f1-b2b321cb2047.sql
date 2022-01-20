CREATE TABLE public."its_pqh_46be1e57-e4ed-4ef7-95f1-b2b321cb2047"
(
    time timestamp with time zone,
    p double precision,
    q double precision,
    heat_demand double precision,
    uuid uuid,
    CONSTRAINT its_pqh_pkey PRIMARY KEY (uuid)
)
    WITH (
        OIDS = FALSE
    )
    TABLESPACE pg_default;

INSERT INTO
    public."its_pqh_46be1e57-e4ed-4ef7-95f1-b2b321cb2047" (uuid, time, p, q, heat_demand)
VALUES
('661ac594-47f0-4442-8d82-bbeede5661f7', '2020-01-01 00:00:00+0', 1000.0, 329.0, 8.0),
('5adcd6c5-a903-433f-b7b5-5fe669a3ed30', '2020-01-01 00:15:00+0', 1250.0, 411.0, 12.0);
