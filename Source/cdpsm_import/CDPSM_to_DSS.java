//  ----------------------------------------------------------
//  Copyright (c) 2009-2011, MelTran, Inc.
//  All rights reserved.
//  ----------------------------------------------------------

// package epri.com.opendss.cim ;

import java.io.*;

import org.apache.jena.ontology.*;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.*;
import org.apache.jena.util.FileManager;

public class CDPSM_to_DSS extends Object {
  static final String nsCIM = "http://iec.ch/TC57/2013/CIM-schema-cim16#";
  static final String nsRDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
  static final String baseURI = "http://opendss";

  static final String combinedOwl = "Combined.owl";

  static String SafeProperty (Resource r, Property p, String def) {
    if (r.hasProperty(p)) return r.getProperty(p).getString();
    return def;
  }

  static String SafePhasesX (Resource r, Property p) {
    if (r.hasProperty(p)) {
      return r.getProperty(p).getObject().toString();
    }
    return "#PhaseCode.ABCN";
  }

  static double SafeDouble (Resource r, Property p, double def) {
    if (r.hasProperty(p)) return r.getProperty(p).getDouble();
    return def;
  }

  static int SafeInt (Resource r, Property p, int def) {
    if (r.hasProperty(p)) return r.getProperty(p).getInt();
    return def;
  }

  static String DSS_Guid (String arg) {
    int hash = arg.lastIndexOf ("#_");
    return arg.substring (hash + 2);
  }

  static String DSS_Name (String arg) {
    String s1 = arg.replace (' ', '_');
    String s2 = s1.replace ('.', '_');
    String s3 = s2.replace ('(', '_');
    String s4 = s3.replace (')', '_');
    return s4.replace ('=', '_');
  }

  static String DSS_ID (String arg) {
    int hash = arg.lastIndexOf ("#");
    return DSS_Name (arg.substring (hash + 1));
  }

  static String SafeResName (Resource r, Property p) {
    String s;
    if (r.hasProperty(p)) {
      s = r.getProperty(p).getString();
    } else {
      s = r.getLocalName();
    }
    return DSS_Name (s);
  }

  static String SafeResourceLookup (Model mdl, Property ptName, Resource r, Property p, String def) {
    if (r.hasProperty(p)) {
      Resource res = mdl.getResource (r.getProperty(p).getResource().toString());
      String s = SafeResName (res, ptName);
      return s;
    }
    return def;
  }

  static int GetMatIdx (int n, int row, int col) {
    int seq = -1;
    int i, j;
    for (j = 0; j < col; j++) {
      seq += (n - j);
    }
    for (i = col; i <= row; i++) {
      ++seq;
    }
    return seq;
  }

  static String GetACLineParameters (Model mdl, Resource r, double len) {
    Property ptR1 = mdl.getProperty (nsCIM, "ACLineSegment.r");
    Property ptR0 = mdl.getProperty (nsCIM, "ACLineSegment.r0");
    Property ptX1 = mdl.getProperty (nsCIM, "ACLineSegment.x");
    Property ptX0 = mdl.getProperty (nsCIM, "ACLineSegment.x0");
    Property ptB1 = mdl.getProperty (nsCIM, "ACLineSegment.bch");
    Property ptB0 = mdl.getProperty (nsCIM, "ACLineSegment.b0ch");

    if (r.hasProperty (ptX1)) {
      double r1 = SafeDouble (r, ptR1, 0) / len;
      double r0 = SafeDouble (r, ptR0, 0) / len;
      double x1 = SafeDouble (r, ptX1, 0) / len;
      double x0 = SafeDouble (r, ptX0, x1) / len;
      double b0 = SafeDouble (r, ptB0, 0) / len; // EdF writes b0ch but not bch
      double b1 = SafeDouble (r, ptB1, b0) / len;
      double c0 = 1.0e9 * b0 / 314.159; // EdF 50-Hz
      double c1 = 1.0e9 * b1 / 314.159; // EdF 50-Hz
      return " r1=" + Double.toString(r1) + " x1=" + Double.toString(x1) + " c1=" + Double.toString(c1) +
             " r0=" + Double.toString(r1) + " x0=" + Double.toString(r1) + " c0=" + Double.toString(c0);
    }
    return "";
  }

  static String GetImpedanceMatrix (Model mdl, Property ptName, Property ptCount, Resource r) {
    int nphases, seq, size, i, j;

    Property ptData = mdl.getProperty (nsCIM, "PhaseImpedanceData.PhaseImpedance");
    Property ptSeq = mdl.getProperty (nsCIM, "PhaseImpedanceData.sequenceNumber");
    Property ptR = mdl.getProperty (nsCIM, "PhaseImpedanceData.r");
    Property ptX = mdl.getProperty (nsCIM, "PhaseImpedanceData.x");
    Property ptB = mdl.getProperty (nsCIM, "PhaseImpedanceData.b");
    nphases = r.getProperty(ptCount).getInt();

    size = 0;
    for (i = 0; i < nphases; i++) {
      for (j = i; j < nphases; j++) {
        ++size;
      }
    }
    double [] rMat = new double [size];
    double [] xMat = new double [size];
    double [] cMat = new double [size];
    for (i = 0; i < size; i++) {
      rMat[i] = 0.0;
      xMat[i] = 0.0;
      cMat[i] = 0.0;
    }
    double len = 1.0; // 5280.0;

    ResIterator iter = mdl.listResourcesWithProperty (ptData, r);
    while (iter.hasNext()) {
      Resource rData = iter.nextResource();
      seq = rData.getProperty(ptSeq).getInt() - 1;  // zero-based arrays in Java, 1-based in CIM
      if (rData.hasProperty(ptR)) {
        rMat[seq] = len * rData.getProperty(ptR).getDouble();
      }
      if (rData.hasProperty(ptX)) {
        xMat[seq] = len * rData.getProperty(ptX).getDouble();
      }
      if (rData.hasProperty(ptB)) {
        cMat[seq] = len * rData.getProperty(ptB).getDouble() * 1.0e9 / 377.0;
      }
    }

    StringBuilder buf;

    buf = new StringBuilder ("nphases=" + Integer.toString(nphases));
    StringBuilder rBuf = new StringBuilder (" rmatrix=[");
    StringBuilder xBuf = new StringBuilder (" xmatrix=[");
    StringBuilder cBuf = new StringBuilder (" cmatrix=[");

    for (i = 0; i < nphases; i++) {  // lower triangular, go across the rows for OpenDSS
      for (j = 0; j <= i; j++) {
        seq = GetMatIdx (nphases, i, j);
        rBuf.append (Double.toString (rMat[seq]) + " ");
        xBuf.append (Double.toString (xMat[seq]) + " ");
        cBuf.append (Double.toString (cMat[seq]) + " ");
      }
      if ((i+1) < nphases) {
        rBuf.append ("| ");
        xBuf.append ("| ");
        cBuf.append ("| ");
      }
    }

    buf.append (rBuf + "]");
    buf.append (xBuf + "]");
    buf.append (cBuf + "]");

    return buf.toString();
  }

  static String Phase_String (String arg) {
    int hash = arg.lastIndexOf ("#PhaseCode.");
    return arg.substring (hash + 11);
  }

  static String Phase_Kind_String (String arg) {
    int hash = arg.lastIndexOf ("#SinglePhaseKind.");
    return arg.substring (hash + 17);
  }

  static int Phase_xCount (String phs, boolean shunt) {
    if (phs.equals ("N")) {
      return 3;
    }
    int cnt = phs.length();
    if (phs.contains ("N")) {
      --cnt;
    } else if (shunt == true) { // shunt without N ==> delta, either 1 or 3 phases
      if (cnt == 2) {
        cnt = 1;
      }
    }
    return cnt;
  }

  static int Phase_Count (String arg, boolean shunt) {
    String phs = Phase_String (arg);
    return Phase_xCount (phs, shunt);
  }

  static String Bus_xPhases (String phs) {
    if (phs.contains ("ABC")) {
      return ".1.2.3";
    } else if (phs.contains ("AB")) {
      return ".1.2";
    } else if (phs.contains ("AC")) {
      return ".1.3";
    } else if (phs.contains ("BC")) {
      return ".2.3";
    } else if (phs.contains ("A")) {
      return ".1";
    } else if (phs.contains ("B")) {
      return ".2";
    } else if (phs.contains ("C")) {
      return ".3";
    } else {
      return "";  // defaults to 3 phases
    }
  }

  static String Bus_Phases (String arg) {
    String phs = Phase_String (arg);
    return Bus_xPhases(phs);
  }

	static String Bus_ShuntPhases (String phs, int phs_cnt, String phs_conn) {
		if (phs_cnt == 3) {
			return ".1.2.3";
		}
		if (phs_conn.contains("w")) {
			return Bus_xPhases(phs);
		}
		if (phs_cnt == 1) {
			if (phs.contains ("A")) {
				return ".1.2";
			} else if (phs.contains ("B")) {
				return ".2.3";
			} else if (phs.contains ("C")) {
				return ".3.1";
			}
		}
		if (phs.contains ("AB")) {
			return ".1.2.3";
		} else if (phs.contains ("AC")) {
			return ".3.1.2";
		}
		// phs.contains ("BC")) for 2-phase delta
		return ".2.3.1";
	}

	static String Shunt_Conn (Resource r, Property p) {
		if (r.hasProperty(p)) {
			String arg = r.getProperty(p).getObject().toString();
			int hash = arg.lastIndexOf ("#PhaseShuntConnectionKind.");
			String conn = arg.substring (hash + 26);
			if (conn.contains ("D")) {
				return "d";
			}
		}
		return "w";
	}

  static String WirePhases (Model mdl, Resource r, Property p1, Property p2) {
    ResIterator it = mdl.listResourcesWithProperty (p1, r);
    if (it.hasNext()) {  // we don't know what order the phases will come
      boolean bA = false;
      boolean bB = false;
      boolean bC = false;
      while (it.hasNext()) {
        Resource rP = it.nextResource();
        if (rP.hasProperty(p2)) {
          String s = Phase_Kind_String (rP.getProperty(p2).getObject().toString());
          if (s.equals("A")) bA = true;
          if (s.equals("B")) bB = true;
          if (s.equals("C")) bC = true;
        }
      }
      StringBuilder buf = new StringBuilder ("");
      if (bA) buf.append ("A");
      if (bB) buf.append ("B");
      if (bC) buf.append ("C");
      return buf.toString();
    }
    return "ABC";
  }

  static int Count_Phases (String arg) {
    String phs = Phase_String (arg);
    if (phs.contains ("ABC")) {
      return 3;
    } else if (phs.contains ("AB")) {
      return 2;
    } else if (phs.contains ("AC")) {
      return 2;
    } else if (phs.contains ("BC")) {
      return 2;
    } else if (phs.contains ("A")) {
      return 1;
    } else if (phs.contains ("B")) {
      return 1;
    } else if (phs.contains ("C")) {
      return 1;
    } else {
      return 3;  // defaults to 3 phases
    }
  }

  static String GetWdgConnection (Resource r, Property p, String def) {
    if (r.hasProperty(p)) {
      String arg = r.getProperty(p).getObject().toString();
      int hash = arg.lastIndexOf ("#WindingConnection.");
      return arg.substring (hash + 19);  // TODO - change Y to W
    }
    return def;
  }

  static String GetPropValue (Model mdl, String uri, String prop) {
    Resource res = mdl.getResource (uri);
    Property p = mdl.getProperty (nsCIM, prop);
    return res.getProperty(p).getString();
  }

  static String GetLoadModel (Model mdl, Resource rLoad) {
    Property ptResponse = mdl.getProperty (nsCIM, "EnergyConsumer.LoadResponse");
    if (rLoad.hasProperty (ptResponse)) {
      Resource rModel = rLoad.getProperty(ptResponse).getResource();
      Property ptPv = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.pVoltageExponent");
      Property ptQv = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.qVoltageExponent");
      Property ptPz = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.pConstantImpedance");
      Property ptPi = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.pConstantCurrent");
      Property ptPp = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.pConstantPower");
      Property ptQz = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.qConstantImpedance");
      Property ptQi = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.qConstantCurrent");
      Property ptQp = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.qConstantPower");
      double Pv = new Double (SafeProperty (rModel, ptPv, "0"));
      double Qv = new Double (SafeProperty (rModel, ptQv, "0"));
      double Pz = new Double (SafeProperty (rModel, ptPz, "0"));
      double Pi = new Double (SafeProperty (rModel, ptPi, "0"));
      double Pp = new Double (SafeProperty (rModel, ptPp, "0"));
      double Qz = new Double (SafeProperty (rModel, ptQz, "0"));
      double Qi = new Double (SafeProperty (rModel, ptQi, "0"));
      double Qp = new Double (SafeProperty (rModel, ptQp, "0"));
      if (Pv == 1 && Qv == 2) {
        return "model=4";
      }
      if (Pz == 100 && Qz == 100) {
        return "model=2";
      }
      if (Pp == 100 && Qz == 100) {
        return "model=3";
      }
      if (Pi == 100 && Qi == 100) {
        return "model=5";
      }
    }
    return "model=1";
  }

  static String GetBusName (Model mdl, String eq_id, int seq) {
    String strSeq = Integer.toString (seq);
    Property ptNode = mdl.getProperty (nsCIM, "Terminal.ConnectivityNode");
    Property ptEquip = mdl.getProperty (nsCIM, "Terminal.ConductingEquipment");
    Property ptSeq = mdl.getProperty (nsCIM, "Terminal.sequenceNumber");
    Property ptName = mdl.getProperty (nsCIM, "IdentifiedObject.name");
    Resource resID = mdl.getResource (eq_id);
    ResIterator iter = mdl.listResourcesWithProperty (ptEquip, resID);
    // if Terminal.sequenceNumbers exist, match to seq argument
    // if not, count the loop and match that to seq
    int idx = 0;
    boolean found = false;
    while (iter.hasNext()) {
      Resource res = iter.nextResource(); // this is a terminal of eq_id
      ++idx;
      if (res.hasProperty (ptSeq)) {
        if (res.hasProperty (ptSeq, strSeq)) {
          found = true;
        }
      } else {
        if (idx == seq) {
          found = true;
        }
      }
      if (found) {
        Resource CN = res.getProperty(ptNode).getResource();
        if (CN.hasProperty(ptName)) {
          return DSS_Name (CN.getProperty(ptName).getString());
        } else {
          return DSS_Name (CN.getLocalName());
        }
      }
    }
    return "x";
  }

  static String GetPowerTransformerData (Model mdl, String xf_id, double smult, double vmult) {

		// used to collect the PowerTransformerEnds belonging to xf_id
		Property ptXfmr = mdl.getProperty (nsCIM, "PowerTransformerEnd.PowerTransformer");
		// PowerTransformerEnd instance data0
    Property ptEndRw = mdl.getProperty (nsCIM, "PowerTransformerEnd.r");
    Property ptEndC = mdl.getProperty (nsCIM, "PowerTransformerEnd.connectionKind");
//		Property ptEndK = mdl.getProperty (nsCIM, "PowerTransformerEnd.phaseAngleClock");

		Property ptEndV = mdl.getProperty (nsCIM, "PowerTransformerEnd.ratedU");
    Property ptEndS = mdl.getProperty (nsCIM, "PowerTransformerEnd.ratedS");
		Property ptEndGrnd = mdl.getProperty (nsCIM, "TransformerEnd.grounded");
    Property ptEndRn   = mdl.getProperty (nsCIM, "TransformerEnd.rground");
    Property ptEndXn   = mdl.getProperty (nsCIM, "TransformerEnd.xground");
    Property ptEndN    = mdl.getProperty (nsCIM, "TransformerEnd.endNumber");

    Resource rXf = mdl.getResource (xf_id);

    // first count the number of windings
    ResIterator itEnd;
    Resource rEnd;
    int i, nwdg = 0;
    itEnd = mdl.listResourcesWithProperty (ptXfmr, rXf);
    while (itEnd.hasNext()) {
      ++nwdg;
      itEnd.nextResource();
    }

		// now go through the PowerTransformerEnds; we can only deal with two or three
		double v[] = new double[nwdg];
		double s[] = new double[nwdg];
		double zb[] = new double[nwdg];
		double rw[] = new double[nwdg];
		double rn[] = new double[nwdg];
		double xn[] = new double[nwdg];
		String wye[] = new String[nwdg];
		Resource rEnds[] = new Resource[nwdg];
    itEnd = mdl.listResourcesWithProperty (ptXfmr, rXf);
    while (itEnd.hasNext()) {
      rEnd = itEnd.nextResource();
      i = SafeInt (rEnd, ptEndN, 1) - 1;
      v[i] = vmult * SafeDouble (rEnd, ptEndV, 1.0 / vmult); // kv
      s[i] = smult * SafeDouble (rEnd, ptEndS, 1.0 / smult); // kva
			zb[i] = 1000.0 * v[i] * v[i] / s[i];
			rw[i] = 100.0 * SafeDouble (rEnd, ptEndRw, 0.0) / zb[i];
      rn[i] = SafeDouble (rEnd, ptEndRn, 0.0);
      xn[i] = SafeDouble (rEnd, ptEndXn, 0.0);
      wye[i] = GetWdgConnection (rEnd, ptEndC, "W");
			rEnds[i] = rEnd; // save to construct the impedance data
    }

    StringBuilder bufU = new StringBuilder (" kvs=[");
    StringBuilder bufS = new StringBuilder (" kvas=[");
    StringBuilder bufC = new StringBuilder (" conns=[");
    StringBuilder bufR = new StringBuilder (" %Rs=[");

    for (i = 0; i < nwdg; i++) {
      String U = Double.toString(v[i]);
      String S = Double.toString(s[i]);
      String R = Double.toString(rw[i]);

      if (i < nwdg - 1) {
        bufU.append (U + ",");
        bufS.append (S + ",");
        bufC.append (wye[i] + ",");
        bufR.append (R + ",");
      } else {
        bufU.append (U + "]");
        bufS.append (S + "]");
        bufC.append (wye[i] + "]");
        bufR.append (R + "]");
      }
    }

		// find the Xhl, Xht, Xlt, and core values from TransformerMeshImpedance, TransformerCoreAdmittance
		Property ptFrom = mdl.getProperty (nsCIM, "TransformerMeshImpedance.FromTransformerEnd");
		Property ptTo = mdl.getProperty (nsCIM, "TransformerMeshImpedance.ToTransformerEnd");
		Property ptMeshX = mdl.getProperty (nsCIM, "TransformerMeshImpedance.x");
		Property ptCoreB = mdl.getProperty (nsCIM, "TransformerCoreAdmittance.b");
		Property ptCoreG = mdl.getProperty (nsCIM, "TransformerCoreAdmittance.g");
		Resource rMesh, rCore;
		double x;

    StringBuilder bufX = new StringBuilder (" %imag=" + Double.toString(0.0) + " %noloadloss=" + Double.toString(0.0));
		itEnd = mdl.listResourcesWithProperty (ptFrom, rEnds[0]);
		while (itEnd.hasNext()) {
			rMesh = itEnd.nextResource();
			x = 100.0 * SafeDouble (rMesh, ptMeshX, 1.0) / zb[0];
			bufX.append (" Xhl=" + Double.toString(x));
		}
    if (nwdg > 2) { // TODO - more than 3 windings
//      bufX.append (" Xht=" + Double.toString(x[0]+x[2]));
//      bufX.append (" Xlt=" + Double.toString(x[1]+x[2]));
    }
    return " phases=3 windings=" + Integer.toString(nwdg) + bufX + bufU + bufS + bufC + bufR;
  }

  static String GetWindingBuses (Model mdl, String xf_id) {
    Property ptXfmr = mdl.getProperty (nsCIM, "PowerTransformerEnd.PowerTransformer");
    Property ptEnd = mdl.getProperty (nsCIM, "TransformerEnd.endNumber");
    Property ptTerm = mdl.getProperty (nsCIM, "TransformerEnd.Terminal");
    Property ptPhs = mdl.getProperty (nsCIM, "ConductingEquipment.phases"); // TODO - not there any longer
    Property ptNode = mdl.getProperty (nsCIM, "Terminal.ConnectivityNode");
    Property ptName = mdl.getProperty (nsCIM, "IdentifiedObject.name");

    // first count the number of windings
    Resource xfRes = mdl.getResource (xf_id);
    ResIterator it = mdl.listResourcesWithProperty (ptXfmr, xfRes);
    String busName;
    int i, nwdg = 0;
    while (it.hasNext()) {
      ++nwdg;
      it.nextResource();
    }
    String bus[] = new String[nwdg];
    String phs[] = new String[nwdg];

    // now pull bus names in correct order, by winding endNumber
    it = mdl.listResourcesWithProperty (ptXfmr, xfRes);
    while (it.hasNext()) {  // pulls all the Ends for xf_id
      Resource wdg = it.nextResource();
      i = SafeInt (wdg, ptEnd, 1) - 1;
      Resource trm = wdg.getProperty(ptTerm).getResource();
      phs[i] = SafePhasesX (trm, ptPhs);
      Resource CN = trm.getProperty(ptNode).getResource();
      if (CN.hasProperty(ptName)) {
        bus[i] = DSS_Name (CN.getProperty(ptName).getString());
      } else {
        bus[i] = DSS_Name (CN.getLocalName());
      }
    }

    StringBuilder buf = new StringBuilder ("[");
    for (i = 0; i < nwdg; i++) {
      buf.append (bus[i]);
      buf.append (Bus_Phases (phs[i]));
      if (i < nwdg-1) {
        buf.append (",");
      } else {
        buf.append ("]");
      }
    }
    return buf.toString();
  }

  static String GetTankBusesAndPhaseCount (Model mdl, Resource xfRes) {
    Property ptXfmr = mdl.getProperty (nsCIM, "TransformerTankEnd.TransformerTank");
    Property ptEnd = mdl.getProperty (nsCIM, "TransformerEnd.endNumber");
    Property ptTerm = mdl.getProperty (nsCIM, "TransformerEnd.Terminal");
    Property ptPhs = mdl.getProperty (nsCIM, "TransformerTankEnd.phases");
    Property ptNode = mdl.getProperty (nsCIM, "Terminal.ConnectivityNode");
    Property ptName = mdl.getProperty (nsCIM, "IdentifiedObject.name");

    // first count the number of windings
    ResIterator it = mdl.listResourcesWithProperty (ptXfmr, xfRes);
    String busName;
    int i, nwdg = 0;
    while (it.hasNext()) {
      ++nwdg;
      it.nextResource();
    }
    String bus[] = new String[nwdg];
    String phs[] = new String[nwdg];
    int n, nphase = 3;

    // now pull bus names in correct order, by winding endNumber
    it = mdl.listResourcesWithProperty (ptXfmr, xfRes);
    while (it.hasNext()) {  // pulls all the Ends for xf_id
      Resource wdg = it.nextResource();
      i = SafeInt (wdg, ptEnd, 1) - 1;
      Resource trm = wdg.getProperty(ptTerm).getResource();
      phs[i] = SafePhasesX (wdg, ptPhs);
      n = Count_Phases (phs[i]);
      if (n < nphase) {
        nphase = n;
      }
      Resource CN = trm.getProperty(ptNode).getResource();
      if (CN.hasProperty(ptName)) {
        bus[i] = DSS_Name (CN.getProperty(ptName).getString());
      } else {
        bus[i] = DSS_Name (CN.getLocalName());
      }
    }

    StringBuilder buf = new StringBuilder (" windings=" + Integer.toString(nwdg) 
                                           + " phases=" + Integer.toString(nphase) + " buses=[");
    for (i = 0; i < nwdg; i++) {
      buf.append (bus[i]);
      buf.append (Bus_Phases (phs[i]));
      if (i < nwdg-1) {
        buf.append (",");
      } else {
        buf.append ("]");
      }
    }
    return buf.toString();
  }

  static String GetTankData (Model mdl, Resource rTank, double smult, double vmult) {
    // used to collect the TransformerTankEnds belonging to rTank
    Property ptXfmr = mdl.getProperty (nsCIM, "TransformerTankEnd.TransformerTank");

    // TODO - core Y and mesh Z option if there is no datasheet
    // TODO - parse the ShortCircuitTest and NoLoadTest data if available

		// navigate from TransformerTank via AssetDatasheet to TransformerTankInfo,
		// then collect its TransformerEndInfos, ShortCircuitTests and NoLoadTests
		Property ptAssetPSR = mdl.getProperty (nsCIM, "Asset.PowerSystemResources");
		Property ptAssetInf = mdl.getProperty (nsCIM, "Asset.AssetInfo");
//    Property ptInf1 = mdl.getProperty (nsCIM, "TransformerTank.TransformerTankInfo");
    Property ptInf2 = mdl.getProperty (nsCIM, "TransformerEndInfo.TransformerTankInfo");

    Property ptEndGrnd = mdl.getProperty (nsCIM, "TransformerEnd.grounded");
    Property ptEndRn   = mdl.getProperty (nsCIM, "TransformerEnd.rground");
    Property ptEndXn   = mdl.getProperty (nsCIM, "TransformerEnd.xground");
    Property ptEndN    = mdl.getProperty (nsCIM, "TransformerEnd.endNumber");
		Property ptEndV    = mdl.getProperty (nsCIM, "TransformerEnd.BaseVoltage");

    Property ptInfR = mdl.getProperty (nsCIM, "TransformerEndInfo.r");
    Property ptInfN = mdl.getProperty (nsCIM, "TransformerEndInfo.endNumber");
    Property ptInfC = mdl.getProperty (nsCIM, "TransformerEndInfo.connectionKind");
		Property ptEndK = mdl.getProperty (nsCIM, "TransformerEndInfo.phaseAngleClock");
    Property ptInfV = mdl.getProperty (nsCIM, "TransformerEndInfo.ratedU");
    Property ptInfS = mdl.getProperty (nsCIM, "TransformerEndInfo.ratedS");
		Property ptInfS1 = mdl.getProperty (nsCIM, "TransformerEndInfo.shortTermS");
		Property ptInfS2 = mdl.getProperty (nsCIM, "TransformerEndInfo.emergencyS");

    // first count the number of windings
    ResIterator itEnd;
    Resource rEnd;
    int i, nwdg = 0;
    itEnd = mdl.listResourcesWithProperty (ptXfmr, rTank);
    while (itEnd.hasNext()) {
      ++nwdg;
      itEnd.nextResource();
    }
    double v[] = new double[nwdg];
    double s[] = new double[nwdg];
    double r[] = new double[nwdg];
    double x[] = new double[nwdg];
    double zb[] = new double[nwdg];
    double g[] = new double[nwdg];
    double b[] = new double[nwdg];
    double rn[] = new double[nwdg];
    double xn[] = new double[nwdg];
    String wye[] = new String[nwdg];
    for (i = 0; i < nwdg; i++) {
      v[i] = 1.0;
      s[i] = 1.0;
      r[i] = 0.0;
      x[i] = 0.01;
      zb[i] = 1.0;
      g[i] = 0.0;
      b[i] = 0.0;
      rn[i] = 0.0;
      xn[i] = 0.0;
      wye[i] = "W";
    }

    // get the available datasheet values first - must be on the tank
    ResIterator itAsset = mdl.listResourcesWithProperty (ptAssetPSR, rTank);
    while (itAsset.hasNext()) {
      Resource rAsset = itAsset.nextResource();
      if (rAsset.hasProperty(ptAssetInf)) {
        Resource rDS = rAsset.getProperty(ptAssetInf).getResource();
        itEnd = mdl.listResourcesWithProperty (ptInf2, rDS);
        while (itEnd.hasNext()) {
          rEnd = itEnd.nextResource();
          if (rEnd.hasProperty(ptInfN)) {
            i = rEnd.getProperty(ptInfN).getInt() - 1;
            v[i] = SafeDouble (rEnd, ptInfV, v[i]);
            s[i] = SafeDouble (rEnd, ptInfS, s[i]);
            r[i] = SafeDouble (rEnd, ptInfR, r[i]);
            wye[i] = GetWdgConnection (rEnd, ptInfC, wye[i]);
          }
        }
      }
    }

    // now go through the TransformerTankEnds and back-fill / over-write the asset data
    itEnd = mdl.listResourcesWithProperty (ptXfmr, rTank);
    while (itEnd.hasNext()) {
      rEnd = itEnd.nextResource();
      i = SafeInt (rEnd, ptEndN, 1) - 1;
      rn[i] = SafeDouble (rEnd, ptEndRn, rn[i]);
      xn[i] = SafeDouble (rEnd, ptEndXn, xn[i]);
    }

    StringBuilder bufU = new StringBuilder (" kvs=[");
    StringBuilder bufS = new StringBuilder (" kvas=[");
    StringBuilder bufC = new StringBuilder (" conns=[");
    StringBuilder bufR = new StringBuilder (" %Rs=[");
    double maxB = 0.0;
    double maxG = 0.0;

    for (i = 0; i < nwdg; i++) {
      s[i] = s[i] * smult;
      v[i] = v[i] * vmult;
      zb[i] = 1000.0 * v[i] * v[i] / s[i];
      r[i] = 100.0 * r[i] / zb[i]; // percent R, X, G, B
      x[i] = 100.0 * x[i] / zb[i];
      g[i] = 100.0 * g[i] * zb[i];
      b[i] = 100.0 * b[i] * zb[i];
      if (g[i] > maxG) {
        maxG = g[i];
      }
      if (b[i] > maxB) {
        maxB = b[i];
      }
      String U = Double.toString(v[i]);
      String S = Double.toString(s[i]);
      String R = Double.toString(r[i]);

      if (i < nwdg - 1) {
        bufU.append (U + ",");
        bufS.append (S + ",");
        bufC.append (wye[i] + ",");
        bufR.append (R + ",");
      } else {
        bufU.append (U + "]");
        bufS.append (S + "]");
        bufC.append (wye[i] + "]");
        bufR.append (R + "]");
      }
    }
    StringBuilder bufX = new StringBuilder (" %imag=" + Double.toString(maxB) + " %noloadloss=" + Double.toString(maxG));
    bufX.append (" Xhl=" + Double.toString(x[0]+x[1]));
    if (nwdg > 2) { // TODO - more than 3 windings
      bufX.append (" Xht=" + Double.toString(x[0]+x[2]));
      bufX.append (" Xlt=" + Double.toString(x[1]+x[2]));
    }
    return bufX.toString() + bufU + bufS + bufC + bufR;
  }

  static String GetRegulatorData (Model mdl, Resource reg) {
    StringBuffer buf = new StringBuffer("");

    // looking for the transformer and the winding number
    Property ptEnd = mdl.getProperty (nsCIM, "RatioTapChanger.TransformerEnd");
    Property ptWdg = mdl.getProperty (nsCIM, "TransformerEnd.endNumber");
		Property ptTank = mdl.getProperty (nsCIM, "TransformerTankEnd.TransformerTank");
		Property ptXf = mdl.getProperty (nsCIM, "TransformerTank.PowerTransformer");
    Property ptName = mdl.getProperty (nsCIM, "IdentifiedObject.name");
    Resource rEnd = mdl.getProperty(reg,ptEnd).getResource();
		int nWdg = SafeInt (rEnd, ptWdg, 1);
		Resource rTank = mdl.getProperty(rEnd,ptTank).getResource();
		Resource rXf = mdl.getProperty(rTank,ptXf).getResource();
    String xfName = mdl.getProperty(rTank,ptName).getString(); // TODO - verify why rXf name prepends =
    buf.append (" transformer=" + xfName + " winding=" + Integer.toString(nWdg));

		// look up the asset datasheet
		double CT = 1.0;
		double PT = 1.0;
		Property ptAssetPSR = mdl.getProperty (nsCIM, "Asset.PowerSystemResources");
		Property ptAssetInf = mdl.getProperty (nsCIM, "Asset.AssetInfo");
		Property ptPT = mdl.getProperty (nsCIM, "TapChangerInfo.ptRatio");
		Property ptCT = mdl.getProperty (nsCIM, "TapChangerInfo.ctRating");
		ResIterator itAsset = mdl.listResourcesWithProperty (ptAssetPSR, reg);
		while (itAsset.hasNext()) {
			Resource rAsset = itAsset.nextResource();
			if (rAsset.hasProperty(ptAssetInf)) {
				Resource rDS = rAsset.getProperty(ptAssetInf).getResource();
				CT = SafeDouble (rDS, ptCT, 1.0);
				PT = SafeDouble (rDS, ptPT, 1.0);
			}
		}

    // look up the control parameters TODO - all the others
		Property ptCtl = mdl.getProperty (nsCIM, "TapChanger.TapChangerControl");
    Property ptBand = mdl.getProperty (nsCIM, "RegulatingControl.targetDeadband");
    Property ptSet = mdl.getProperty (nsCIM, "RegulatingControl.targetValue");
    Property ptR = mdl.getProperty (nsCIM, "TapChangerControl.lineDropX");
    Property ptX = mdl.getProperty (nsCIM, "TapChangerControl.lineDropR");
		Resource ctl = mdl.getProperty(reg,ptCtl).getResource();
    double ldcR = SafeDouble (ctl, ptR, 0.0);
    double ldcX = SafeDouble (ctl, ptX, 0.0);
    double Vreg = SafeDouble (ctl, ptSet, 120.0);
    double Vband = SafeDouble (ctl, ptBand, 2.0);
    Vreg /= PT;
    Vband /= PT;

		// TODO - pull SvTapStep

    buf.append (" ctprim=" + Double.toString(CT) +
                " ptratio=" + Double.toString(PT) +
                " vreg=" + Double.toString(Vreg) +
                " band=" + Double.toString(Vband) +
                " r=" + Double.toString(ldcR) +
                " x=" + Double.toString(ldcX));
    return buf.toString();
  }

  static String GetXfmrCode (Model mdl, String id) {  // TODO - 2011 instance files don't actually use SC/NL tests
    Property ptInfo = mdl.getProperty (nsCIM, "TransformerEndInfo.TransformerTankInfo");
    Property ptU = mdl.getProperty (nsCIM, "TransformerEndInfo.ratedU");
    Property ptS = mdl.getProperty (nsCIM, "TransformerEndInfo.ratedS");
    Property ptR = mdl.getProperty (nsCIM, "TransformerEndInfo.r");
    Property ptC = mdl.getProperty (nsCIM, "TransformerEndInfo.connectionKind");
    Property ptFrom = mdl.getProperty (nsCIM, "DistributionWindingTest.EnergisedEnd");
    Property ptType = mdl.getProperty (nsRDF, "type");
    Property ptTo = mdl.getProperty (nsCIM, "ShortCircuitTest.FromWinding"); // TODO - handle more than 3 windings
    Property ptNLL = mdl.getProperty (nsCIM, "NoLoadTest.loss");
    Property ptImag = mdl.getProperty (nsCIM, "OpenCircuitTest.excitingCurrent");
    Property ptZsc = mdl.getProperty (nsCIM, "ShortCircuitTest.leakageImpedance");
    StringBuilder bufU = new StringBuilder ("kvs=[");
    StringBuilder bufS = new StringBuilder ("kvas=[");
    StringBuilder bufC = new StringBuilder ("conns=[");
    StringBuilder bufR = new StringBuilder ("%Rs=[");
    StringBuilder bufOC = new StringBuilder ("");
    StringBuilder bufSC = new StringBuilder ("");
    String sPhases = "phases=3 ";
    int nWindings = 0;
    int nOffset = nsRDF.length() - 2;

    Resource xfRes = mdl.getResource (id);
    ResIterator iter = mdl.listResourcesWithProperty (ptInfo, xfRes);
    while (iter.hasNext()) {
      Resource wdg = iter.nextResource();
      ++nWindings;
      double dU = SafeDouble (wdg, ptU, 1);
      double dS = SafeDouble (wdg, ptS, 1);
      double dR = SafeDouble (wdg, ptR, 0);
      double Zbase = 1000.0 * dU * dU / dS;
      dR = 100.0 * dR / Zbase;
      String U = Double.toString(dU);
      String S = Double.toString(dS);
      String R = Double.toString(dR);
      String C = GetWdgConnection (wdg, ptC, "W");
      if (C.equals ("I")) {
        sPhases = "phases=1 ";
        C = "W";
      }
      if (iter.hasNext()) {
        bufU.append (U + ",");
        bufS.append (S + ",");
        bufC.append (C + ",");
        bufR.append (R + ",");
      } else {
        bufU.append (U + "] ");
        bufS.append (S + "] ");
        bufC.append (C + "] ");
        bufR.append (R + "] ");
      }
      ResIterator iterTest = mdl.listResourcesWithProperty (ptFrom, wdg);
      while (iterTest.hasNext()) {
        Resource test = iterTest.nextResource();
        String sType = test.getProperty (ptType).getObject().toString().substring(nOffset);
        if (sType.equals("NoLoadTest")) {
          double dNLL = SafeDouble (test, ptNLL, 0);
          double dImag = SafeDouble (test, ptImag, 0);
          dNLL = 100 * dNLL / dS;
          bufOC.append ("%imag=" + Double.toString(dImag) + " %noloadloss=" + Double.toString(dNLL) + " ");
        } else if (sType.equals("ShortCircuitTest")) {
          double dXsc = SafeDouble (test, ptZsc, 0.0001);
          dXsc = 100.0 * dXsc / Zbase;
          bufSC.append ("Xhl=" + Double.toString(dXsc) + " ");
        }
      }
    }
    if (bufSC.length() < 1) {
      bufSC.append ("xhl=1.0 ");
    }
    String sWindings = "windings=" + Integer.toString(nWindings) + " ";
    return sWindings + sPhases + bufSC.toString() + bufOC.toString() + bufU.toString() + bufS.toString() + bufC.toString() + bufR.toString();
  }

  static String GetBusPositionString (Model mdl, String id) {
    Property ptX = mdl.getProperty (nsCIM, "PositionPoint.xPosition");
    Property ptY = mdl.getProperty (nsCIM, "PositionPoint.yPosition");
    Property ptPosSeq = mdl.getProperty (nsCIM, "PositionPoint.sequenceNumber");
    Property ptLoc = mdl.getProperty (nsCIM, "PositionPoint.Location");
    Property ptGeo = mdl.getProperty (nsCIM, "PowerSystemResource.Location");

    Property ptBank = mdl.getProperty (nsCIM, "DistributionTransformer.TransformerBank");
    Property ptXfmr = mdl.getProperty (nsCIM, "DistributionTransformerWinding.Transformer");

    Property ptNode = mdl.getProperty (nsCIM, "Terminal.ConnectivityNode");
    Property ptTrmSeq = mdl.getProperty (nsCIM, "Terminal.sequenceNumber");
    Property ptEquip = mdl.getProperty (nsCIM, "Terminal.ConductingEquipment");

    // for drilling Eq=>VoltageLevel=>Sub=>Geo, or Eq=>Line=>Geo
    Property ptCont = mdl.getProperty (nsCIM, "Equipment.EquipmentContainer");
    Property ptSub = mdl.getProperty (nsCIM, "VoltageLevel.Substation");

    Resource bus = mdl.getResource (id);
    Resource trm, eq;
    String trmSeq = "1";

    Resource geo = null;
    Resource refGeo = null; // bank, line, or substation

    // first look for a terminal equipment that directly has a GeoLocation
    //   but the GeoLocation could also be on a TransformerBank, Line, or Substation
    ResIterator terms = mdl.listResourcesWithProperty (ptNode, bus);
    while (terms.hasNext() && geo == null) {
      trm = terms.nextResource();
      eq = trm.getProperty(ptEquip).getResource();
      if (eq.hasProperty (ptGeo)) {
        geo = eq.getProperty(ptGeo).getResource();
        trmSeq = SafeProperty (trm, ptTrmSeq, "1");
      } else if (eq.hasProperty (ptXfmr)) {
        Resource xf = eq.getProperty (ptXfmr).getResource();
        if (xf.hasProperty (ptBank)) {
          Resource bank = xf.getProperty(ptBank).getResource();
          if (bank.hasProperty (ptGeo)) {
            refGeo = bank.getProperty(ptGeo).getResource();
          }
        }
      } else if (eq.hasProperty (ptCont)) {
        Resource rcont = eq.getProperty(ptCont).getResource();
        if (rcont.hasProperty(ptGeo)) {
          refGeo = rcont.getProperty(ptGeo).getResource();
        } else if (rcont.hasProperty (ptSub)) {
          Resource rsub = eq.getProperty(ptSub).getResource();
          if (rsub.hasProperty(ptGeo)) {
            refGeo = rsub.getProperty(ptGeo).getResource();
          }
        }
      }
    }
    if (geo == null) {
      geo = refGeo;
    }

    if (geo != null) {
      ResIterator iter = mdl.listResourcesWithProperty (ptLoc, geo);
      Resource pos = null;
      while (iter.hasNext()) {
        pos = iter.nextResource();
        if (pos.hasProperty (ptPosSeq, trmSeq)) { // at the end we are looking for
          return pos.getProperty(ptX).getString() + ", " + pos.getProperty(ptY).getString();
        }
      }
      if (pos != null) {
        return pos.getProperty(ptX).getString() + ", " + pos.getProperty(ptY).getString();
      }
    } else {
 //     System.out.println (" NO GEO FOUND");
    }

    return "";
  }

  static String FindConductorAmps (Model mdl, Resource res, Property ptDataSheet, Property ptAmps) {
    double iMin = 1.0;
    double iVal;
    if (res.hasProperty(ptDataSheet)) {
      Resource rInf = res.getProperty(ptDataSheet).getResource();
      if (rInf.hasProperty(ptAmps)) {
        iVal = SafeDouble (rInf, ptAmps, 0.0);
        if (iVal > iMin) {
          iMin = iVal;
        }
      }
    }
    return " normamps=" + Double.toString(iMin);
  } 

  static double FindBaseVoltage (Resource res, Property ptEquip, Property ptEqBaseV, Property ptLevBaseV, Property ptBaseNomV) {
    Resource rBase = null;
    if (res.hasProperty (ptEqBaseV)) {
      rBase = res.getProperty(ptEqBaseV).getResource();
    } else if (res.hasProperty (ptEquip)) {
      Resource rEquip = res.getProperty(ptEquip).getResource();
      if (rEquip.hasProperty(ptEqBaseV)) {
        rBase = rEquip.getProperty(ptEqBaseV).getResource();
      } else if (rEquip.hasProperty(ptLevBaseV)) {
        rBase = rEquip.getProperty(ptLevBaseV).getResource();
      }
    }
    if (rBase != null) {
      return SafeDouble (rBase, ptBaseNomV, 1.0);
    }
    return 1.0;
  }

  public static void main (String args[]) throws UnsupportedEncodingException, FileNotFoundException {

    String fProfile = "", fName = "", fOut = "", fBus = "", fGuid = "", fEnc = "";
    double freq = 60.0, vmult = 0.001, smult = 0.001;
    int fInFile = 0;
    int fNameSeq = 0;

    if (args.length < 3) {
      System.out.println ("Usage: CDPSM_to_DSS [options] input.xml output_root");
      System.out.println ("       -p={c|a|f|e|g|s|t} // profile; only supports Combined for now");
      System.out.println ("       -e={u|i}           // encoding; UTF-8 or ISO-8859-1");
      System.out.println ("       -f={50|60}         // system frequency");
      System.out.println ("       -v={1|0.001}       // multiplier that converts voltage to kV for OpenDSS");
      System.out.println ("       -s={1000|1|0.001}  // multiplier that converts p,q,s to kVA for OpenDSS");
      System.out.println ("       -q={y|n}           // are unique names used?");
    }
    int i = 0;
    while (i < args.length) {
      if (args[i].charAt(0) == '-') {
        char opt = args[i].charAt(1);
        String optVal = args[i].substring(3);
        if (opt == 'p') {
          fProfile = combinedOwl;
        } else if (opt=='e') {
          if (optVal.charAt(0) == 'u') {
            fEnc = "UTF8";
          } else {
            fEnc = "ISO-8859-1";
          }
        } else if (opt=='q') {
          if (optVal.charAt(0) == 'y') {
            fNameSeq = 0;
          } else {
            fNameSeq = 1;
          }
        } else if (opt=='f') {
          freq = Double.parseDouble(optVal);
        } else if (opt=='v') {
          vmult = Double.parseDouble(optVal);
        } else if (opt=='s') {
          smult = Double.parseDouble(optVal);
        }
      } else if (fInFile < 1) {
        fInFile = 1;
        fName = args[i];
      } else {
        fOut = args[i] + "_base.dss";
        fBus = args[i] + "_busxy.dss";
        fGuid = args[i] + "_guids.dss";
      }
      ++i;
    }

    System.out.println (fEnc + " f=" + Double.toString(freq) + " v="  + Double.toString(vmult) + " s=" + Double.toString(smult));

//    ModelMaker maker = ModelFactory.createFileModelMaker (fProfile);
//    Model tmpModel = maker.createDefaultModel();
//    Model model = ModelFactory.createOntologyModel (OntModelSpec.OWL_DL_MEM, tmpModel);
		Model model = ModelFactory.createOntologyModel (OntModelSpec.OWL_DL_MEM);
       
    InputStream in = FileManager.get().open(fName);
    if (in == null) {
      throw new IllegalArgumentException( "File: " + fName + " not found");
    }
        
    PrintWriter out = new PrintWriter (fOut);
    PrintWriter outBus = new PrintWriter (fBus);
    PrintWriter outGuid = new PrintWriter (fGuid);

    model.read(new InputStreamReader(in, fEnc), baseURI, "RDF/XML");
        
    String qPrefix = "PREFIX r: <" + nsRDF + "> PREFIX c: <" + nsCIM + "> ";
    Query query;
    QueryExecution qexec;
    ResultSet results;
    QuerySolution soln;
    Resource res;
    String id, name, phs, bus_phs, bus1, bus2, phs_conn;
    int phs_cnt;
    Property ptName = model.getProperty (nsCIM, "IdentifiedObject.name");
    Property ptType = model.getProperty (nsRDF, "type");
    Property ptOpen = model.getProperty (nsCIM, "Switch.normalOpen");

    Property ptEqBaseV = model.getProperty (nsCIM, "ConductingEquipment.BaseVoltage"); 
    Property ptLevBaseV = model.getProperty (nsCIM, "VoltageLevel.BaseVoltage"); 
    Property ptEquip = model.getProperty (nsCIM, "Equipment.EquipmentContainer");
    Property ptBaseNomV = model.getProperty (nsCIM, "BaseVoltage.nominalVoltage");

    // Dump all the GeoLocation references
    /*
    Property ptGeo = model.getProperty (nsCIM, "PowerSystemResource.GeoLocation");
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:GeoLocation}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      res = model.getResource (id);
      name = SafeResName (res, ptName);
      ResIterator it = model.listResourcesWithProperty (ptGeo, res);
      while (it.hasNext()) {
        Resource rEq = it.nextResource();
        String sType = rEq.getProperty(ptType).getObject().toString();
        outBus.println ("// " + name + "==>" + sType + ":" + SafeResName(rEq, ptName));
      }
    }
    outBus.println ();
    */

    // ConnectivityNode ==> bus coordinate CSV 
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:ConnectivityNode}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      res = model.getResource (id);
      name = SafeResName (res, ptName);
      String strPos = GetBusPositionString (model, id);
      if (strPos.length() > 0) {
        outBus.println (name + ", " + strPos);
      } else {
        outBus.println ("// " + name + ", *****");
      }
    }
    outBus.println ();
    outBus.close ();
    
    // EnergySource ==> Circuit
    int NumCircuits = 0;
    int NumSources = 0;

    out.println ("clear");
    query = QueryFactory.create (qPrefix + "select ?s ?name ?v ?ckt where {?s r:type c:EnergySource. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:EnergySource.voltageMagnitude ?v;" +
                                 "   c:Equipment.EquipmentContainer ?ckt" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptESr0 = model.getProperty (nsCIM, "EnergySource.r0");
    Property ptESr1 = model.getProperty (nsCIM, "EnergySource.r");
    Property ptESx0 = model.getProperty (nsCIM, "EnergySource.x0");
    Property ptESx1 = model.getProperty (nsCIM, "EnergySource.x");
    Property ptESVnom = model.getProperty (nsCIM, "EnergySource.nominalVoltage");
    Property ptESVmag = model.getProperty (nsCIM, "EnergySource.voltageMagnitude");
    Property ptESVang = model.getProperty (nsCIM, "EnergySource.voltageAngle");
    while (results.hasNext()) {
      soln = results.next();
      ++NumSources;

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      String vSrce = soln.get ("?v").toString();
      String ckt = soln.get ("?ckt").toString();

      res = model.getResource (id);

      double vmag = vmult * SafeDouble (res, ptESVmag, 1.0);
      double vnom = vmult * SafeDouble (res, ptESVnom, vmag);
      double vang = SafeDouble (res, ptESVang, 0.0) * 57.3;
      double r0 = SafeDouble (res, ptESr0, 0.0);
      double r1 = SafeDouble (res, ptESr1, 0.0);
      double x1 = SafeDouble (res, ptESx1, 0.001);
      double x0 = SafeDouble (res, ptESx0, x1);
      double vpu = vmag / vnom;

      bus1 = GetBusName (model, id, 1); // TODO - no phase model

      String srcClass = "Vsource.";
      if (NumCircuits < 1) { // name.equals ("source")
        srcClass = "Circuit.";
        name = DSS_Name (GetPropValue (model, ckt, "IdentifiedObject.name"));
        NumCircuits = 1;
      } else if (name.equals("source")) {
        name = "_" + name;
      }

      out.println ("new " + srcClass + name + " phases=3 bus1=" + bus1 + 
                   " basekv=" + Double.toString(vnom) + " pu=" + Double.toString(vpu) + " angle=" + Double.toString(vang) +
                   " r0=" + Double.toString(r0) + " r1=" + Double.toString(r1) +
                   " x0=" + Double.toString(x0) + " x1=" + Double.toString(x1));
      outGuid.println (srcClass + name + "\t" + DSS_Guid (id));
    }
    if (NumCircuits < 1) {  // try the first breaker
      query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:Breaker}");
      qexec = QueryExecutionFactory.create (query, model);
      results=qexec.execSelect();
      while (results.hasNext()) {
        soln = results.next();
        id = soln.get ("?s").toString();

        res = model.getResource (id);
        bus1 = GetBusName (model, id, 1);

        name = SafeResName (res, ptName);
        out.println ("new Circuit." + name + " phases=3 bus1=" + bus1 + " basekv=1");
//        outGuid.println ("Circuit." + name + "\t" + DSS_Guid (id));
      }
    }

    out.println ("// set frequency=" + Double.toString(freq));

    // SynchronousMachine ==> Generator
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:SynchronousMachine}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptGenS = model.getProperty (nsCIM, "GeneratingUnit.ratedNetMaxP");
    Property ptGenP = model.getProperty (nsCIM, "GeneratingUnit.initialP");
    Property ptGenRef = model.getProperty (nsCIM, "SynchronousMachine.GeneratingUnit");
    Property ptGenQ = model.getProperty (nsCIM, "SynchronousMachine.baseQ");
    Property ptGenQmin = model.getProperty (nsCIM, "SynchronousMachine.minQ");
    Property ptGenQmax = model.getProperty (nsCIM, "SynchronousMachine.maxQ");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();

      res = model.getResource (id);
			// TODO - generators need phase modeling as well
      bus1 = GetBusName (model, id, 1); // + bus_phs;
      name = SafeResName (res, ptName);
      Resource resUnit = res.getProperty (ptGenRef).getResource();

      double genS = SafeDouble (resUnit, ptGenS, 1.0) * 1000.0;  // assume MW per CPSM
      double genP = SafeDouble (resUnit, ptGenP, 1.0) * 1000.0;
      double genQ = SafeDouble (res, ptGenQ, 0.0) * 1000.0;
      double genQmin = SafeDouble (res, ptGenQmin, 0.44 * genS) * 1000.0 * -1.0;
      double genQmax = SafeDouble (res, ptGenQmax, 0.44 * genS) * 1000.0;
      double genKv = vmult * FindBaseVoltage (res, ptEquip, ptEqBaseV, ptLevBaseV, ptBaseNomV);

      out.println ("new Generator." + name + " phases=3 bus1=" + bus1 + 
                   " conn=w kva=" + Double.toString (genS) + " kw=" + Double.toString (genP) + 
                   " kvar=" + Double.toString (genQ) + " minkvar=" + Double.toString (genQmin) + 
                   " maxkvar=" + Double.toString (genQmax) + " kv=" + Double.toString (genKv));
      outGuid.println ("Load." + name + "\t" + DSS_Guid (id));
    }

    // EnergyConsumer ==> Load
    double total_load_kw = 0.0;
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:EnergyConsumer}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptP = model.getProperty (nsCIM, "EnergyConsumer.pfixed");
    Property ptQ = model.getProperty (nsCIM, "EnergyConsumer.qfixed");
    Property ptCust = model.getProperty (nsCIM, "EnergyConsumer.customerCount");
		Property ptPhsLoad1 = model.getProperty (nsCIM, "EnergyConsumerPhase.EnergyConsumer");
		Property ptPhsLoad2 = model.getProperty (nsCIM, "EnergyConsumerPhase.phase");
		Property ptConnLoad = model.getProperty (nsCIM, "EnergyConsumer.phaseConnection");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();

      res = model.getResource (id);
			phs = WirePhases (model, res, ptPhsLoad1, ptPhsLoad2);
			phs_cnt = Phase_xCount (phs, true);
			phs_conn = Shunt_Conn (res, ptConnLoad);
			bus_phs = Bus_ShuntPhases (phs, phs_cnt, phs_conn);
      bus1 = GetBusName (model, id, 1) + bus_phs;

      name = SafeResName (res, ptName);
      double pL = SafeDouble (res, ptP, 1);
      double qL = SafeDouble (res, ptQ, 0);
      pL *= smult;
      qL *= smult;
      total_load_kw += pL;
      String pLoad = Double.toString(pL);
      String qLoad = Double.toString(qL);
      String nCust = SafeProperty (res, ptCust, "1");
      String loadModel = GetLoadModel (model, res);
      double loadKv = vmult * FindBaseVoltage (res, ptEquip, ptEqBaseV, ptLevBaseV, ptBaseNomV);
			if ((phs_cnt < 3) && phs_conn.contains("w")) {
				loadKv /= Math.sqrt(3.0);
			}

      out.println ("new Load." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + 
                   " conn=" + phs_conn + " kw=" + pLoad + " kvar=" + qLoad + " numcust=" + nCust + 
                   " kv=" + Double.toString(loadKv) + " " + loadModel);
      outGuid.println ("Load." + name + "\t" + DSS_Guid (id));
    }
    out.println ();
    out.println ("// total load = " + Double.toString (total_load_kw) + " kW");

    // LinearShuntCompensator ==> Capacitor
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:LinearShuntCompensator. " + 
                                 "?s c:IdentifiedObject.name ?name}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptSecB = model.getProperty (nsCIM, "LinearShuntCompensator.bPerSection");
    Property ptSecN = model.getProperty (nsCIM, "LinearShuntCompensator.normalSections");
    Property ptNumSteps = model.getProperty (nsCIM, "ShuntCompensator.maximumSections");
		Property ptPhsShunt1 = model.getProperty (nsCIM, "LinearShuntCompensatorPhase.ShuntCompensator");
		Property ptPhsShunt2 = model.getProperty (nsCIM, "ShuntCompensatorPhase.phase");
		Property ptConnShunt = model.getProperty (nsCIM, "ShuntCompensator.phaseConnection");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);
			phs = WirePhases (model, res, ptPhsShunt1, ptPhsShunt2);
			phs_cnt = Phase_xCount (phs, true);
			phs_conn = Shunt_Conn (res, ptConnShunt);
			bus_phs = Bus_ShuntPhases (phs, phs_cnt, phs_conn);
			bus1 = GetBusName (model, id, 1) + bus_phs;

			String numSteps = SafeProperty (res, ptNumSteps, "1");
      double cap_b = SafeInt (res, ptNumSteps, 1) * SafeDouble (res, ptSecB, 0.0001);
      double cap_v = vmult * FindBaseVoltage (res, ptEquip, ptEqBaseV, ptLevBaseV, ptBaseNomV);
			if ((phs_cnt < 3) && phs_conn.contains("w")) {
				cap_v /= Math.sqrt(3.0);
			}
			String nomU = Double.toString(cap_v);
      String nomQ = Double.toString(cap_v * cap_v * cap_b * 1000.0);

      out.println ("new Capacitor." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + 
                   " conn=" + phs_conn + " numsteps=" + numSteps + " kv=" + nomU + " kvar=" + nomQ);
      outGuid.println ("Capacitor." + name + "\t" + DSS_Guid (id));
    }


    // WireData
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:WireInfo}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptGMR = model.getProperty (nsCIM, "WireInfo.gmr");
    Property ptWireRadius = model.getProperty (nsCIM, "WireInfo.radius");
    Property ptWireDiameter = model.getProperty (nsCIM, "WireInfo.diameter");
    Property ptWireCurrent = model.getProperty (nsCIM, "WireInfo.ratedCurrent");
    Property ptWireR25 = model.getProperty (nsCIM, "WireInfo.rAC25");
    Property ptWireR50 = model.getProperty (nsCIM, "WireInfo.rAC50");
    Property ptWireR75 = model.getProperty (nsCIM, "WireInfo.rAC75");
    Property ptWireRdc = model.getProperty (nsCIM, "WireInfo.rDC20");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      res = model.getResource (id);
      name = SafeResName (res, ptName);

      double normamps = SafeDouble (res, ptWireCurrent, 0.0);

      double radius = SafeDouble (res, ptWireRadius, 0.0);
      if (radius <= 0) {
        radius = 0.5 * SafeDouble (res, ptWireDiameter, 0.0);
      }

      double gmr = SafeDouble (res, ptGMR, 0.0);
      if (gmr <= 0) {
        gmr = 0.7788 * radius;
      }

      double wireRac = SafeDouble (res, ptWireR50, 0.0);
      if (wireRac <= 0) {
        wireRac = SafeDouble (res, ptWireR25, 0.0);
      }
      if (wireRac <= 0) {
        wireRac = SafeDouble (res, ptWireR75, 0.0);
      }
      double wireRdc = SafeDouble (res, ptWireRdc, 0.0);
      if (wireRdc <= 0) {
        wireRdc = wireRac;
      } else if (wireRac <= 0) {
        wireRac = wireRdc;
      }

      if (radius > 0.0 && gmr > 0.0) { // don't write WireData if it's just used for ratedCurrent
        out.println ("new WireData." + name + " gmr=" + Double.toString(gmr) + " radius=" + Double.toString(radius) +
                     " rac=" + Double.toString(wireRac) + " rdc=" + Double.toString(wireRdc) + " normamps=" + Double.toString(normamps) + 
                     " Runits=m Radunits=m gmrunits=m");
        outGuid.println ("WireData." + name + "\t" + DSS_Guid (id));
      }
    }

    // LineGeometries
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:OverheadConductorInfo. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptWireX = model.getProperty (nsCIM, "WireArrangement.mountingPointX");
    Property ptWireY = model.getProperty (nsCIM, "WireArrangement.mountingPointY");
    Property ptWireP = model.getProperty (nsCIM, "WireArrangement.position");
//    Property ptWireInfo = model.getProperty (nsCIM, "WireArrangement.WireInfo");
    Property ptWireInfo = model.getProperty (nsCIM, "WireArrangement.ConductorInfo");
    Property ptGeoPhases = model.getProperty (nsCIM, "ConductorInfo.phaseCount");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);

      int nconds = 0;
      int icond;
      String WireInfo, wireX, wireY;
      StringBuffer buf = new StringBuffer();
      int nphases = SafeInt (res, ptGeoPhases, 1);
      ResIterator wIter = model.listResourcesWithProperty (ptWireInfo, res);
      while (wIter.hasNext()) {
        Resource wa = wIter.nextResource();
        icond = SafeInt (wa, ptWireP, 1);
        if (icond > nconds) {
          nconds = icond;
        }
        wireX = SafeProperty (wa, ptWireX, "0");
        wireY = SafeProperty (wa, ptWireY, "0");
        WireInfo = SafeResourceLookup (model, ptName, wa, ptWireInfo, "**");
        buf.append ("~ cond=" + Integer.toString(icond) + " wire=" + WireInfo + " x=" + wireX + " h=" + wireY + "\n");
      }

      if (nconds > 0 && nphases > 0) {
        out.println ("new LineGeometry." + name + " nconds=" + Integer.toString(nconds) + " nphases=" + Integer.toString(nphases) + 
                     " reduce=y units=ft");
        out.println (buf.toString());
        outGuid.println ("LineGeometry." + name + "\t" + DSS_Guid (id));
      }
    }

    // LineCodes
    int NumLineCodes = 0;
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:PerLengthPhaseImpedance. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      ++NumLineCodes;

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);
      Property ptCount = model.getProperty (nsCIM, "PerLengthPhaseImpedance.conductorCount");
      String zMat = "nphases=3 r0=0 r1=0 x0=0.001 x1=0.001 c0=0 c1=0";
      if (res.hasProperty (ptCount)) {
        zMat = GetImpedanceMatrix (model, ptName, ptCount, res);
      }

      out.println ("new LineCode." + name + " " + zMat);
      outGuid.println ("LineCode." + name + "\t" + DSS_Guid (id));
    }
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:PerLengthSequenceImpedance. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptSeqR1 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.r");
    Property ptSeqR0 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.r0");
    Property ptSeqX1 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.x");
    Property ptSeqX0 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.x0");
    Property ptSeqB1 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.bch");
    Property ptSeqB0 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.b0ch");
    while (results.hasNext()) {
      soln = results.next();
      ++NumLineCodes;

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);

      double sqR1 = SafeDouble (res, ptSeqR1, 0);
      double sqR0 = SafeDouble (res, ptSeqR0, 0);
      double sqX1 = SafeDouble (res, ptSeqX1, 0);
      double sqX0 = SafeDouble (res, ptSeqX0, 0);
      if (sqR0 <= 0) {
        sqR0 = sqR1;
      }
      if (sqX0 <= 0) {
        sqX0 = sqX1;
      }
      String seqR1 = Double.toString(sqR1);
      String seqR0 = Double.toString(sqR0);
      String seqX1 = Double.toString(sqX1);
      String seqX0 = Double.toString(sqX0);

      double bch = SafeDouble (res, ptSeqB1, 0);
      String seqC1 = Double.toString(bch * 1.0e9 / 314.0);  // TODO: only for EdF during 2009 interop tests
      bch = SafeDouble (res, ptSeqB0, 0);
      String seqC0 = Double.toString(bch * 1.0e9 / 314.0);  // TODO: only for EdF during 2009 interop tests

      out.println ("new LineCode." + name + " nphases=3 r1=" + seqR1 + " x1=" + seqX1 + " c1=" + seqC1 +
                   " r0=" + seqR0 + " x0=" + seqX0 + " c0=" + seqC0);
      outGuid.println ("LineCode." + name + "\t" + DSS_Guid (id));
    }
    if (NumLineCodes < 1) {
      out.println ("new LineCode.dummy_linecode_1 nphases=1 rmatrix={0} xmatrix={0.001} cmatrix={0}");
      out.println ("new LineCode.dummy_linecode_2 nphases=2 rmatrix={0|0 0} xmatrix={0.001|0 0.001} cmatrix={0|0 0}");
      out.println ("new LineCode.dummy_linecode_3 nphases=3 r1=0 x1=0.001 c1=0 r0=0 x0=0.001 c0=0");
      // TODO - do we want this GUID or not?
    }

    // ACLineSegment ==> Line
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name ?len where {?s r:type c:ACLineSegment. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:Conductor.length ?len" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptPhsZ = model.getProperty (nsCIM, "ACLineSegment.PerLengthImpedance");
    Property ptLineLen = model.getProperty (nsCIM, "Conductor.length");
    Property ptDataSheet = model.getProperty (nsCIM, "PowerSystemResource.AssetDatasheet");
    Property ptAmps = model.getProperty (nsCIM, "WireInfo.ratedCurrent");
    Property ptPhsLine1 = model.getProperty (nsCIM, "ACLineSegmentPhase.ACLineSegment");
    Property ptPhsLine2 = model.getProperty (nsCIM, "ACLineSegmentPhase.phase");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      if (fNameSeq > 0) {
        name = DSS_ID (id);
      } else {
        name = DSS_Name (soln.get ("?name").toString());
      }
      res = model.getResource (id);
      String len = soln.get ("?len").toString();
      phs = WirePhases (model, res, ptPhsLine1, ptPhsLine2);
      phs_cnt = Phase_xCount (phs, false);
      bus_phs = Bus_xPhases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;
      bus2 = GetBusName (model, id, 2) + bus_phs;
      double dLen = SafeDouble (res, ptLineLen, 1.0);

      String zPhase = SafeResourceLookup (model, ptName, res, ptPhsZ, "");
      String zParms = GetACLineParameters (model, res, dLen);
      String linecode = "";
      if (zPhase.length() > 0) {
        linecode = " linecode=" + zPhase;
//      } else if (zSequence.length() > 0) {
//        linecode = " linecode=" + zSequence;
//      } else if (zParms.length() < 1 && zInfo.length() > 0) {
//        linecode = " geometry=" + zInfo;
      } else if (zParms.length() > 0) {
        linecode = zParms;
      } else if (phs_cnt == 1) {
        linecode = " linecode=dummy_linecode_1";
      } else if (phs_cnt == 2) {
        linecode = " linecode=dummy_linecode_2";
      } else {
        linecode = " linecode=dummy_linecode_3";
      }
      String zAmps = "";
      if (zParms.length () > 0) {
        zAmps = FindConductorAmps (model, res, ptDataSheet, ptAmps);
      }

      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
                          + " length=" + len + linecode + zAmps);
      outGuid.println ("Line." + name + "\t" + DSS_Guid (id));
    }

    // LoadBreakSwitch ==> Line switch=y
    query = QueryFactory.create (qPrefix + "select ?s ?name ?open where {?s r:type c:LoadBreakSwitch. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:Switch.normalOpen ?open" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptPhsSwt1 = model.getProperty (nsCIM, "SwitchPhase.Switch");
    Property ptPhsSwt2 = model.getProperty (nsCIM, "SwitchPhase.phaseSide1"); // TODO - phaseSide2?
    if (results.hasNext()) {
			out.println ();
      out.println ("// Load Break Switches");
    }
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);
      phs = WirePhases (model, res, ptPhsSwt1, ptPhsSwt2);
      phs_cnt = Phase_xCount (phs, false);
      bus_phs = Bus_xPhases (phs);
      String open = soln.get ("?open").toString();

      bus1 = GetBusName (model, id, 1) + bus_phs;
      bus2 = GetBusName (model, id, 2) + bus_phs;

      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
                          + " switch=y // CIM LoadBreakSwitch");
      if (open.equals("false")) {
        out.println ("  close Line." + name + " 1");
      } else {
        out.println ("  open Line." + name + " 1");
      }
      outGuid.println ("Line." + name + "\t" + DSS_Guid (id));
    }

    // Fuse ==> Line switch=y
    query = QueryFactory.create (qPrefix + "select ?s ?name ?open where {?s r:type c:Fuse. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:Switch.normalOpen ?open" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    if (results.hasNext()) {
      out.println ();
      out.println ("// Fuses");
    }
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);
      phs = WirePhases (model, res, ptPhsSwt1, ptPhsSwt2);
      phs_cnt = Phase_xCount (phs, false);
      bus_phs = Bus_xPhases (phs);
      String open = soln.get ("?open").toString();

      bus1 = GetBusName (model, id, 1) + bus_phs;
      bus2 = GetBusName (model, id, 2) + bus_phs;

      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
                          + " switch=y // CIM Fuse");
      if (open.equals("false")) {
        out.println ("  close Line." + name + " 1");
      } else {
        out.println ("  open Line." + name + " 1");
      }
      outGuid.println ("Line." + name + "\t" + DSS_Guid (id));
    }

    // Breaker ==> Line switch=y  (NOTE: a source may be attached to the first instance)
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:Breaker. " + 
                                 "?s c:IdentifiedObject.name ?name}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    if (results.hasNext()) {
      out.println ();
      out.println ("// Breakers");
    }
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);
      phs = WirePhases (model, res, ptPhsSwt1, ptPhsSwt2);
      phs_cnt = Phase_xCount (phs, false);
      bus_phs = Bus_xPhases (phs);
      String open = SafeProperty (res, ptOpen, "false");

      bus1 = GetBusName (model, id, 1) + bus_phs;
      bus2 = GetBusName (model, id, 2) + bus_phs;

      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
                          + " switch=y // CIM Breaker");
      if (open.equals("false")) {
        out.println ("  close Line." + name + " 1");
      } else {
        out.println ("  open Line." + name + " 1");
      }
      outGuid.println ("Line." + name + "\t" + DSS_Guid (id));
    }

    // Disconnector ==> Line switch=y
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:Disconnector. " + 
                                 "?s c:IdentifiedObject.name ?name}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    if (results.hasNext()) {
      out.println ();
      out.println ("// Disconnectors");
    }
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);
      phs = WirePhases (model, res, ptPhsSwt1, ptPhsSwt2);
      phs_cnt = Phase_xCount (phs, false);
      bus_phs = Bus_xPhases (phs);
      String open = SafeProperty (res, ptOpen, "false");

      bus1 = GetBusName (model, id, 1) + bus_phs;
      bus2 = GetBusName (model, id, 2) + bus_phs;

      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
                          + " switch=y // CIM Disconnector");
      if (open.equals("false")) {
        out.println ("  close Line." + name + " 1");
      } else {
        out.println ("  open Line." + name + " 1");
      }
      outGuid.println ("Line." + name + "\t" + DSS_Guid (id));
    }

    // Transformer Codes
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:TransformerTankInfo. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
// in 2011 GE and EdF instance files, we don't really have XfmrCodes
//      out.println ("new XfmrCode." + name + " " + GetXfmrCode (model, id));
//      outGuid.println ("XfmrCode." + name + "\t" + DSS_Guid (id));
    }

    // Transformers
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:PowerTransformer. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);

      String xfmrbank;
      String xfBus;

      xfmrbank = " bank=" + name;

      // using tanks or not?
      Property ptTank = model.getProperty (nsCIM, "TransformerTank.PowerTransformer");
      ResIterator itTank = model.listResourcesWithProperty (ptTank, res);
      if (itTank.hasNext()) { // write all the tanks to this bank
        while (itTank.hasNext()) {
          Resource rTank = itTank.nextResource();
          name = DSS_Name (rTank.getProperty(ptName).getString());
          xfBus = GetTankBusesAndPhaseCount (model, rTank);
          out.println ("new Transformer." + name + xfmrbank + xfBus);
          out.println ("~ " + GetTankData (model, rTank, smult, vmult) + " // Tanked");
          outGuid.println ("Transformer." + name + "\t" + DSS_Guid (id));
        }
      } else { // standalone power transformer
        xfBus = GetWindingBuses (model, id);
        out.println ("new Transformer." + name + xfmrbank + " buses=" 
                     + xfBus + "\n ~ " + GetPowerTransformerData (model, id, smult, vmult) + " // Standalone");
        outGuid.println ("Transformer." + name + "\t" + DSS_Guid (id));
      }
    }

    // TODO - regulators were not updated or tested in 2011
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:RatioTapChanger. " + 
                                 "?s c:IdentifiedObject.name ?name}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);
      String sReg = GetRegulatorData (model, res);

      out.println ("new RegControl." + name + " " + sReg);
      outGuid.println ("RegControl." + name + "\t" + DSS_Guid (id));
    }

    // unsupported stuff - TODO - add Jumper and Disconnector
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:Junction}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      res = model.getResource (id);
      name = SafeResName (res, ptName);
      out.println ("// new Junction." + name);
    }
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:BusbarSection}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      res = model.getResource (id);
      name = SafeResName (res, ptName);
      out.println ("// new BusbarSection." + name);
    }
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:Bay}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      res = model.getResource (id);
      name = SafeResName (res, ptName);
      out.println ("// new Bay." + name);
    }

		// wrapup
		query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:BaseVoltage}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
		if (results.hasNext()) {
			out.print ("set voltagebases=[");
		} else {
			out.println ("set voltagebases=[1.0]");
		}
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      res = model.getResource (id);
			double vnom = vmult * SafeDouble (res, ptBaseNomV, 1.0);
			out.print (Double.toString(vnom));
			if (results.hasNext()) {
				out.print (", ");
			} else {
				out.println ("]");
			}
    }
    out.println ("calcv");
    out.println ("buscoords " + fBus);
		out.println ("// guids " + fGuid);
    out.close ();

    outGuid.println ();
    outGuid.close ();
  }
}

