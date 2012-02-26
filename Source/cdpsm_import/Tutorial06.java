// HP example, converted to Windows XP installation with windows-1252 file encoding

import com.hp.hpl.jena.rdf.model.*;
import com.hp.hpl.jena.util.FileManager;
import com.hp.hpl.jena.vocabulary.*;

import java.io.*;

public class Tutorial06 extends Object {
  static final String inputFileName = "C:\\Jena-2.6.2\\doc\\tutorial\\RDQL\\vc-db-1.rdf";
  static final String johnSmithURI = "http://somewhere/JohnSmith/";
    
  public static void main (String args[]) throws UnsupportedEncodingException {
    Model model = ModelFactory.createDefaultModel();
       
    // use the FileManager to find the input file
    InputStream in = FileManager.get().open(inputFileName);
    if (in == null) {
      throw new IllegalArgumentException( "File: " + inputFileName + " not found");
    }
        
    model.read(new InputStreamReader(in, "UTF8"), "");
        
    Resource vcard = model.getResource(johnSmithURI);

    Resource name = (Resource) vcard.getRequiredProperty(VCARD.N).getObject();
    String fullName = vcard.getRequiredProperty(VCARD.FN).getString();

    // add two nick name properties to vcard
    vcard.addProperty(VCARD.NICKNAME, "Smithy")
         .addProperty(VCARD.NICKNAME, "Adman");
        
    // set up the output
    System.out.println("The nicknames of \"" + fullName + "\" are:");
    // list the nicknames
    StmtIterator iter = vcard.listProperties(VCARD.NICKNAME);
    while (iter.hasNext()) {
      System.out.println("    " + iter.nextStatement().getObject().toString());
    }
  }
}

