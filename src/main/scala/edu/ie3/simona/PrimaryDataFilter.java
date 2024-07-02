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

public class PrimaryDataFilter {
  public static void main(String[] args) {

    Path path = Path.of(".", "input", "ma_thesis", "fullGrid", "primary");
    List<File> files = Arrays.stream(Objects.requireNonNull(path.toFile().listFiles())).toList();

    files.forEach(
        file -> {
          try {
            BufferedReader reader = new BufferedReader(new FileReader(file));
            String headline = reader.readLine();
            List<String> lines = reader.lines().toList();

            String txt =
                lines.stream()
                    .filter(line -> line.contains("2016-07-") || line.contains("2016-08-01"))
                    .reduce(headline, (a, b) -> a + "\n" + b);

            Writer writer = new OutputStreamWriter(new FileOutputStream(file));
            writer.write(txt);
            writer.close();
          } catch (Exception e) {
            throw new RuntimeException(e);
          }
        });
  }
}
