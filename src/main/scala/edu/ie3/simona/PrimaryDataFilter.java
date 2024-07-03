/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona;

import java.io.*;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

public class PrimaryDataFilter {
  public static void main(String[] args) {
    Path base = Path.of(".", "input", "ma_thesis", "fullGrid");

    Path input = base.resolve("primary");
    Path output = base.resolve( "primary-x1.5");
    List<File> files = Arrays.stream(Objects.requireNonNull(input.toFile().listFiles())).toList();

      AtomicInteger i = new AtomicInteger(0);

    files.forEach(file -> {
        i.addAndGet(1);
        System.out.println(i.get());

          try {
            BufferedReader reader = new BufferedReader(new FileReader(file));
            String headline = reader.readLine();
            List<String> lines = reader.lines().toList();


            Stream<String> stream = lines.stream()
                //.filter(line ->line.contains("2016-07-") || line.contains("2016-08-01"))
                .map(line -> multiply(line, 1.5));

            String txt = stream.reduce(headline, (a, b) -> a + "\n" + b);

            Writer writer = new OutputStreamWriter(new FileOutputStream(output.resolve(file.getName()).toFile()));
            writer.write(txt);
            writer.close();
          } catch (Exception e) {
            throw new RuntimeException(e);
          }
        });
  }

  public static String multiply(String str, double factor) {
      String[] arr = str.split(",");

      if (arr.length == 2) {
          double modified = Double.parseDouble(arr[0]) * factor;

          return modified +","+arr[1];
      } else if (arr.length == 3) {
          double modified1 = Double.parseDouble(arr[0]) * factor;
          double modified2 = Double.parseDouble(arr[1]) * factor;

          return modified1 +","+ modified2 +","+arr[2];
      } else {
          throw new RuntimeException("Size:"+arr.length);
      }
  }
}
