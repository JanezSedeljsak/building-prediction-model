import java.util.*;
import java.io.*;
import java.text.SimpleDateFormat;

class ConsumptionOnDate {
    Date d;
    Double c;

    ConsumptionOnDate(Date d, Double c, Calendar cal) {
        // add 7 days to date (faster way of filtering later on)
        cal.setTime(d);
        cal.add(Calendar.DATE, 7);
        this.d = cal.getTime();
        this.c = c;
    }
}

public class Parse {
    static Calendar cal;
    static Map<Integer, ArrayList<ConsumptionOnDate>> consumptionHistory;

    static boolean isWeekend(Date d) {
        cal.setTime(d);
        int day = cal.get(Calendar.DAY_OF_WEEK);
        return day == Calendar.SATURDAY || day == Calendar.SUNDAY;
    }

    static String removeQuotations(String str) {
        return str.substring(1, str.length()-1);
    }

    static String getSeason(Date d) {
        cal.setTime(d);
        int day = cal.get(Calendar.DAY_OF_YEAR);
        if (day < 81 || day > 355) return "zima";
        if (day < 173) return "spomlad";
        if (day < 267) return "poletje";
        return "jesen";
    }

    static Double[] calcPastConusmption(int buildingNum, double currentConsumption, Date d) {
        ArrayList<ConsumptionOnDate> currentVals;
        if (consumptionHistory.containsKey(buildingNum)) {
            currentVals = consumptionHistory.get(buildingNum);
            currentVals.removeIf(cd -> cd.d.compareTo(d) < 0);
            if (currentVals.size() == 0) {
                currentVals.add(new ConsumptionOnDate(d, currentConsumption, cal));
                return new Double[] {null, null};   
            }

            ConsumptionOnDate lastRecord = currentVals.get(currentVals.size() - 1);
            Double yesterday = null;
            cal.setTime(d);
            cal.add(Calendar.DATE, 6); // add 6 because of previously added offset
            if (lastRecord.d.compareTo(cal.getTime()) == 0) {
                yesterday = lastRecord.c;
            }

            
            if (currentVals.size() < 4) {
                currentVals.add(new ConsumptionOnDate(d, currentConsumption, cal));
                return new Double[] {yesterday, null};
            }

            double val = .0;
            int amount = 0;
            for (ConsumptionOnDate cd: currentVals) {
                amount++;
                val += cd.c;
            }

            currentVals.add(new ConsumptionOnDate(d, currentConsumption, cal));
            Double pastWeek = amount > 0 ? val / (double)amount : null;
            return new Double[] {yesterday, pastWeek};
        }
        
        currentVals = new ArrayList<>();
        currentVals.add(new ConsumptionOnDate(d, currentConsumption, cal));
        consumptionHistory.put(buildingNum, currentVals);
        return new Double[] {null, null};
    }

    static void readAndWrite(String file, String outFile) throws Exception {
        PrintWriter writer = new PrintWriter(outFile, "UTF-8");
        cal = Calendar.getInstance();
        consumptionHistory = new HashMap<>();

        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
        FileInputStream fis = new FileInputStream(file);
        Scanner sc = new Scanner(fis);
        writer.write("\"regija\",\"stavba\",\"namembnost\",\"povrsina\",\"leto_izgradnje\",\"temp_zraka\",\"temp_rosisca\",\"oblacnost\",\"padavine\",\"pritisk\",\"smer_vetra\",\"hitrost_vetra\",\"poraba\",\"vikend\",\"sezona\",\"tedenska_poraba\",\"vcerajsnja_poraba\"");
        sc.nextLine(); // remove head
        
        while (sc.hasNextLine()) {
            String[] curLine = sc.nextLine().split(",");
            Date date = format.parse(removeQuotations(curLine[0]));

            Double[] pastVals = calcPastConusmption(Integer.parseInt(curLine[2]), Double.parseDouble(curLine[13]), date);
            boolean isWeekend = isWeekend(date);
            String season = getSeason(date);
            int rain = Integer.parseInt(curLine[9]);
            if (rain == -1) rain = 1;

            //if (sevenDayAvg == null) continue;
            writer.write(String.format(
                "\n%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,\"%s\",%s,%s",
                curLine[1],curLine[2],curLine[3],curLine[4],curLine[5],curLine[6],
                curLine[7],curLine[8],rain,curLine[10],curLine[11],curLine[12],curLine[13],
                isWeekend ? "1" : "0", 
                season, 
                pastVals[1] != null ? pastVals[1] : -1,
                pastVals[0] != null ? pastVals[0] : -1
            ));
        }
        
        writer.close();
        sc.close();
    }

    public static void main(String[] args) throws Exception {
        if (args.length == 2) {
            readAndWrite(args[0], args[1]);
        } else {
            System.out.println("Starting Ucna.....");
            readAndWrite("ucnaSem1.txt", "ucna.txt");
            System.out.println("Done with Ucna..... Starting Test");
            readAndWrite("testnaSem1.txt", "test.txt");
            System.out.println("Fully done.....");
        }
        
    }
}