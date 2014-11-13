import java.net.*;
import java.io.*;
import java.nio.*;
import java.nio.channels.*;
import java.util.*;
import java.lang.System; 
public class Generate{ 

	void run(){ 
		try { 
            String[] color = {"red", "orange", "green", "blue", "indigo", "violet"}; 
            String[] lineColor = {"#ff0000", "#ffb000", "#00ff00", "#0000ff", "#440088", "#d02090"}; 
			String ans = "msc { \n hscale = \"2\";\n"; 
			File folder = new File(new File(".").getCanonicalPath()+"/files"); 
            System.out.println(new File(".").getCanonicalPath()+"/files"); 
            System.out.println(System.getProperty("user.dir")); 
            System.out.print(folder.listFiles()); 
            for(final File fileEntry : folder.listFiles()) { 
                String fileName = fileEntry.getName();
                String nodeName =  "\"" + fileName.substring(0,fileName.length()-4) + "\"";  
                ans += nodeName + ","; 
            } 
            ans = ans.substring(0, ans.length()-1);
            ans += ";\n"; 

                int count = 0; 
			for (final File fileEntry : folder.listFiles()) {
				String nodeName = fileEntry.getName();
				FileReader fstream;
				fstream = new FileReader(fileEntry.getAbsoluteFile());
				BufferedReader reader = new BufferedReader(fstream);
				String line;
                int countColor = count%(color.length); 
                String text = "textcolour=\"" + color[countColor] + "\","; 
                String linec = "linecolour=\"" + lineColor[countColor] + "\"";  
                count++; 
				while((line=reader.readLine())!=null){
					String[] parts=line.split("\\s+");
					if(parts[4].equals("sent_to")){
						//the message was sent by this node to another one
						String sender = " \"" + nodeName.split("\\.")[0] + "\" ";
						String receiver = " \"" + parts[5] + "\" ";
						String message = parts[1];
						int logicalTime = Integer.parseInt(parts[7]);//logical using lampart clock
						Long physicalTime = Long.parseLong(parts[9]);//physical clock was obtained using system.getcurrenttime 
                        
                        String part = sender + "->" + receiver + " [ label = \"" + message + "\", " + text + linec + " ];\n"; 
                        ans += part; 
					}
					else{
						//the message was received at this node from another node
						String sender =  " \"" + parts[5] +  "\" ";
						String receiver =  " \"" + nodeName.split("\\.")[0] +  "\" ";
						String message = parts[1];
						int logicalTime = Integer.parseInt(parts[7]);//logical time at which message received using lampart clock
						int senderLogicalTime = Integer.parseInt(parts[9]); // this is the logical time at which message was sent from source node
						Long physicalTime = Long.parseLong(parts[11]);//physical clock was obtained using system.getcurrenttime
					    	
                        String part = sender + "->" + receiver + " [ label = \"" + message + "\", " + text + linec + " ];\n"; 
                        ans += part; 
					}
				}
				reader.close();
			}
        ans += "}\n"; 
        Writer writer = null; 
	    writer = new BufferedWriter( new OutputStreamWriter(new FileOutputStream("filename.txt"), "utf-8")); 
        writer.write(ans);  
        writer.close(); 
        
        
        }catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        } 
    public static void main(String args[]){ 
        Generate g = new Generate(); 
        g.run(); 
    }
} 

