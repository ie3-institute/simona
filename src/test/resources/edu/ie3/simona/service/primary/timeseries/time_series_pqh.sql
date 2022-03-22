CREATE TABLE public.time_series_pqh
(
    uuid uuid PRIMARY KEY,
    time_series uuid NOT NULL,
    time timestamp with time zone NOT NULL,
    p double precision NOT NULL,
    q double precision NOT NULL,
    heat_demand double precision NOT NULL
)
    WITHOUT OIDS
    TABLESPACE pg_default;

CREATE INDEX time_series_pqh_series_id ON time_series_pqh USING hash (time_series);

-- Order of columns is important when using btree: https://www.postgresql.org/docs/14/indexes-multicolumn.html
-- time_series at first since we at most use an equality constraint on time_series and a range query on time
CREATE UNIQUE INDEX time_series_pqh_series_time ON time_series_pqh USING btree (time_series, time);

INSERT INTO
    public.time_series_pqh (uuid, time_series, time, p, q, heat_demand)
VALUES
('661ac594-47f0-4442-8d82-bbeede5661f7', '46be1e57-e4ed-4ef7-95f1-b2b321cb2047', '2020-01-01 00:00:00+0', 1000.0, 329.0, 8.0),
('5adcd6c5-a903-433f-b7b5-5fe669a3ed30', '46be1e57-e4ed-4ef7-95f1-b2b321cb2047', '2020-01-01 00:15:00+0', 1250.0, 411.0, 12.0);
