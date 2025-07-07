package edu.ie3.simona.recode;

import edu.ie3.datamodel.exceptions.EntityProcessorException;
import edu.ie3.datamodel.exceptions.ProcessorProviderException;
import edu.ie3.datamodel.io.connectors.InfluxDbConnector;
import edu.ie3.datamodel.io.processor.ProcessorProvider;
import edu.ie3.datamodel.io.sink.OutputDataSink;
import edu.ie3.datamodel.models.Entity;
import edu.ie3.datamodel.models.result.ResultEntity;
import edu.ie3.datamodel.models.timeseries.TimeSeries;
import edu.ie3.datamodel.models.timeseries.TimeSeriesEntry;
import edu.ie3.datamodel.models.value.Value;
import org.influxdb.dto.BatchPoints;
import org.influxdb.dto.Point;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.TimeUnit;

public class RecodeInfluxDBSink implements OutputDataSink {
    public static final Logger log = LoggerFactory.getLogger(RecodeInfluxDBSink.class);
    private final InfluxDbConnector connector;
    private final RecodeDatabaseNamingStrategy entityPersistenceNamingStrategy;
    private final ProcessorProvider processorProvider;


    public RecodeInfluxDBSink(InfluxDbConnector connector, RecodeDatabaseNamingStrategy entityPersistenceNamingStrategy) throws EntityProcessorException {
        this.connector = connector;
        this.entityPersistenceNamingStrategy = entityPersistenceNamingStrategy;
        this.processorProvider = new ProcessorProvider(ProcessorProvider.allResultEntityProcessors(), ProcessorProvider.allTimeSeriesProcessors());

    }

    @Override
    public void shutdown() {
        connector.shutdown();
    }

    @Override
    public <C extends Entity> void persist(C entity) throws ProcessorProviderException {
        Set<Point> points = extractPoints(entity);
        if (points.size() == 1) {
            this.write(points.iterator().next());
        } else {
            this.writeAll(points);
        }
    }

    @Override
    public <C extends Entity> void persistAll(Collection<C> entities) throws ProcessorProviderException {
        Set<Point> points = new HashSet<>();

        for(C entity : entities) {
            points.addAll(this.extractPoints(entity));
        }

        writeAll(points);
    }

    @Override
    public <E extends TimeSeriesEntry<V>, V extends Value, R extends Value> void persistTimeSeries(TimeSeries<E, V, R> timeSeries) {
        log.warn("Persisting time series is not supported!");
    }

    private Point transformToPoint(ResultEntity entity) throws ProcessorProviderException {
        Optional<String> measurementName = entityPersistenceNamingStrategy.getResultEntityName(entity.getClass());
        
        if (measurementName.isEmpty()) {
            log.warn("I could not get a measurement name for class {}. I am using its simple name instead.", entity.getClass().getSimpleName());
        }

        return transformToPoint(entity, measurementName.orElse(entity.getClass().getSimpleName()));
    }

    private Point transformToPoint(ResultEntity entity, String measurementName) throws ProcessorProviderException {
        LinkedHashMap<String, String> entityFieldData = processorProvider.handleEntity(entity).getOrThrow();
        
        if (entityFieldData.containsKey("p")) {
            String value = entityFieldData.remove("p");
            entityFieldData.put("p_mw", value);
        }
        if (entityFieldData.containsKey("q")) {
            String value = entityFieldData.remove("q");
            entityFieldData.put("q_mvar", value);
        }
        
        entityFieldData.remove("time");
        return Point.measurement(transformToMeasurementName(measurementName))
                .time(entity.getTime().toInstant().toEpochMilli(), TimeUnit.MILLISECONDS)
                .tag("input_model", entityFieldData.remove("inputModel"))
                .tag("run", connector.getScenarioName())
                .fields(Collections.unmodifiableMap(entityFieldData))
                .build();
    }

    private <C extends Entity> Set<Point> extractPoints(C entity) throws ProcessorProviderException {
        Set<Point> points = new HashSet<>();
        if (entity instanceof ResultEntity resultEntity) {
            points.add(transformToPoint(resultEntity));
        } else {
            log.error("I don't know how to handle an entity of class {}", entity.getClass().getSimpleName());
        }

        return points;
    }

    private void write(Point point) {
        if (point != null) {
            connector.getSession().write(point);
        }
    }

    private void writeAll(Collection<Point> points) {
        if (!points.isEmpty()) {
            BatchPoints batchPoints = BatchPoints.builder().points(points).build();
            connector.getSession().write(batchPoints);
        }
    }

    private static String transformToMeasurementName(String filename) {
        return filename.trim().replaceAll("\\W", "_");
    }
}
