//  ----------------------------------------------------------
//  Copyright (c) 2016, Battelle Memorial Institute
//  portions Copyright (c) 2009-2011, MelTran, Inc.
//  All rights reserved.
//  ----------------------------------------------------------

// package pnnl.gov.gridlabd.cim ;

// additions 11/22/2016: aVRDelay, TopologicalNodes, TopologicalIslands
// removals  11/22/2016: targetValueUnitMultiplier

import java.io.*;
import java.util.HashMap;

import org.apache.jena.ontology.*;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.*;
import org.apache.jena.util.FileManager;

import org.apache.commons.math3.complex.Complex;
import org.apache.commons.math3.complex.ComplexFormat;

public class CDPSM_to_GLM extends Object {
  static final String nsCIM = "http://iec.ch/TC57/2012/CIM-schema-cim16#";
  static final String nsRDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
  static final String baseURI = "http://gridlabd";

  static final String combinedOwl = "Combined.owl";

	static final Complex pos120 = new Complex (-0.5, 0.5 * Math.sqrt(3.0));
	static final Complex neg120 = new Complex (-0.5, -0.5 * Math.sqrt(3.0));
	static String CFormat (Complex c) {
		String sgn;
		if (c.getImaginary() < 0.0)	{
			sgn = "-";
		} else {
			sgn = "+";
		}
		return String.format("%6g", c.getReal()) + sgn + String.format("%6g", Math.abs(c.getImaginary())) + "j";
	}

	// helper class to keep track of the conductor counts for WireSpacingInfo instances
	static class SpacingCount {
    private final int nconds;
    private final int nphases;

    public SpacingCount(int nconds, int nphases) {
        this.nconds = nconds;
        this.nphases = nphases;
    }

    public int getNumConductors() {
        return nconds;
    }

    public int getNumPhases() {
        return nphases;
    }
	}

	static HashMap<String,SpacingCount> mapSpacings = new HashMap<>();

	// helper class to accumulate nodes and loads
	// all EnergyConsumer data will be attached to node objects, then written as load objects
	//   this preserves the input ConnectivityNode names
	// TODO - another option is to leave all nodes un-loaded,
	//   and attach all loads to parent nodes, closer to what OpenDSS does
	static class GldNode {
		public final String name;
		public String phases;  // ABC allowed
		public double nomvln;  // this is always line-to-neutral
		// for zip load parameters
		public double pa_z;
		public double pb_z;
		public double pc_z;
		public double qa_z;
		public double qb_z;
		public double qc_z;
		public double pa_i;
		public double pb_i;
		public double pc_i;
		public double qa_i;
		public double qb_i;
		public double qc_i;
		public double pa_p;
		public double pb_p;
		public double pc_p;
		public double qa_p;
		public double qb_p;
		public double qc_p;
		// secondary (i.e. triplex) zip loads
		public double p1_z;
		public double p2_z;
		public double q1_z;
		public double q2_z;
		public double p1_i;
		public double p2_i;
		public double q1_i;
		public double q2_i;
		public double p1_p;
		public double p2_p;
		public double q1_p;
		public double q2_p;
		public boolean bDelta;  // will add N or D phasing, if not S
		public boolean bSwing;
		public boolean bSecondary; // i.e. AS, BS or CS even though the load phasing is 1, 2

		public GldNode(String name) {
			this.name = name;
			nomvln = -1.0;
			phases = "";
			pa_z = pb_z = pc_z = qa_z = qb_z = qc_z = 0.0;
			pa_i = pb_i = pc_i = qa_i = qb_i = qc_i = 0.0;
			pa_p = pb_p = pc_p = qa_p = qb_p = qc_p = 0.0;
			p1_z = p2_z = q1_z = q2_z = 0.0;
			p1_i = p2_i = q1_i = q2_i = 0.0;
			p1_p = p2_p = q1_p = q2_p = 0.0;
			bDelta = false;
			bSwing = false;
			bSecondary = false;
		}

		public boolean AddPhases(String phs) {
			StringBuilder buf = new StringBuilder("");
			if (phases.contains("A") || phs.contains("A")) buf.append("A");
			if (phases.contains("B") || phs.contains("B")) buf.append("B");
			if (phases.contains("C") || phs.contains("C")) buf.append("C");
			if (phs.contains("s")) bSecondary = true;
			if (phs.contains("S")) bSecondary = true;
			if (phs.contains("D")) bDelta = true;
			phases = buf.toString();
			return true;
		}

		public String GetPhases() {
			if (bDelta && !bSecondary) return phases + "D";
			if (bSecondary) return phases + "S";
			return phases + "N";
		}

		public boolean HasLoad() {
			if (pa_z != 0.0) return true;
			if (pb_z != 0.0) return true;
			if (pc_z != 0.0) return true;
			if (qa_z != 0.0) return true;
			if (qb_z != 0.0) return true;
			if (qc_z != 0.0) return true;
			if (pa_i != 0.0) return true;
			if (pb_i != 0.0) return true;
			if (pc_i != 0.0) return true;
			if (qa_i != 0.0) return true;
			if (qb_i != 0.0) return true;
			if (qc_i != 0.0) return true;
			if (pa_p != 0.0) return true;
			if (pb_p != 0.0) return true;
			if (pc_p != 0.0) return true;
			if (qa_p != 0.0) return true;
			if (qb_p != 0.0) return true;
			if (qc_p != 0.0) return true;
			if (p1_z != 0.0) return true;
			if (p2_z != 0.0) return true;
			if (q1_z != 0.0) return true;
			if (q2_z != 0.0) return true;
			if (p1_i != 0.0) return true;
			if (p2_i != 0.0) return true;
			if (q1_i != 0.0) return true;
			if (q2_i != 0.0) return true;
			if (p1_p != 0.0) return true;
			if (p2_p != 0.0) return true;
			if (q1_p != 0.0) return true;
			if (q2_p != 0.0) return true;
			return false;
		}
	}
	static HashMap<String,GldNode> mapNodes = new HashMap<>();

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

	static String SafeRegulatingMode (Resource r, Property p, String def) {
		if (r.hasProperty(p)) {
			String arg = r.getProperty(p).getObject().toString();
			int hash = arg.lastIndexOf ("#RegulatingControlModeKind.");
			return arg.substring (hash + 27);
		}
		return def;
	}

	static String GLDCapMode (String s) {  // GLD conversion
		if (s.equals("currentFlow")) return "CURRENT";
    if (s.equals("voltage")) return "VOLT";
    if (s.equals("reactivePower")) return "VAR";
    if (s.equals("timeScheduled")) return "MANUAL";  // TODO - support in GridLAB-D?
    if (s.equals("powerFactor")) return "MANUAL";  // TODO - support in GridLAB-D?
    if (s.equals("userDefined")) return "MANUAL"; 
		return "time";
	}

  static double SafeDouble (Resource r, Property p, double def) {
    if (r.hasProperty(p)) return r.getProperty(p).getDouble();
    return def;
  }

  static int SafeInt (Resource r, Property p, int def) {
    if (r.hasProperty(p)) return r.getProperty(p).getInt();
    return def;
  }

	static boolean SafeBoolean (Resource r, Property p, boolean def) {
		if (r.hasProperty(p)) return r.getProperty(p).getString().equals("true");
		return def;
	}

	static String GetEquipmentType (Resource r) {  // GLD conversion
		// TODO: .listRDFTypes() might be more robust
		String s = r.as(OntResource.class).getRDFType().toString();
		int hash = s.lastIndexOf ("#");
		String t = s.substring (hash + 1);
		if (t.equals("LinearShuntCompensator")) return "cap";
		if (t.equals("ACLineSegment")) return "line"; // assumes we prefix both overhead and underground with line_
		if (t.equals("EnergyConsumer")) return "";  // TODO should we name load:?
		if (t.equals("PowerTransformer")) return "xf";
		return "##UNKNOWN##";
	}

	static String GldPrefixedNodeName (String arg) {
		return "nd_" + arg;
	}

  static String GLD_Name (String arg, boolean bus) {  // GLD conversion
    String s = arg.replace (' ', '_');
    s = s.replace ('.', '_');
		s = s.replace ('=', '_');
		s = s.replace ('+', '_');
		s = s.replace ('^', '_');
		s = s.replace ('$', '_');
		s = s.replace ('*', '_');
		s = s.replace ('|', '_');
		s = s.replace ('[', '_');
		s = s.replace (']', '_');
		s = s.replace ('{', '_');
		s = s.replace ('}', '_');
    s = s.replace ('(', '_');
    s = s.replace (')', '_');
		if (bus) return GldPrefixedNodeName (s);
		return s;
  }

  static String GLD_ID (String arg) {  // GLD conversion
    int hash = arg.lastIndexOf ("#");
    return GLD_Name (arg.substring (hash + 1), false);
  }

  static String SafeResName (Resource r, Property p) {
    String s;
    if (r.hasProperty(p)) {
      s = r.getProperty(p).getString();
    } else {
      s = r.getLocalName();
    }
    return GLD_Name (s, false);
  }

  static String SafeResourceLookup (Model mdl, Property ptName, Resource r, Property p, String def) {
    if (r.hasProperty(p)) {
      Resource res = mdl.getResource (r.getProperty(p).getResource().toString());
      String s = SafeResName (res, ptName);
      return s;
    }
    return def;
  }

	// only valid for the lower triangle
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

	// we have to write 3 of these in the case of 1-phase or 2-phase matrix
  static String GetImpedanceMatrix (Model mdl, String name, Property ptCount, Resource r) {  // TODO - line ratings?
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
    double len = 1609.344; // want ohms/mile and nF/mile

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

		StringBuilder buf = new StringBuilder ("");

		if (nphases == 1) {
			seq = GetMatIdx(nphases, 0, 0);
			buf.append ("object line_configuration {\n  name \"lcon_" + name + "_A\";\n");
			buf.append ("  z11 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c11 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("}\n");
			buf.append ("object line_configuration {\n  name \"lcon_" + name + "_B\";\n");
			buf.append ("  z22 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c22 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("}\n");
			buf.append ("object line_configuration {\n  name \"lcon_" + name + "_C\";\n");
			buf.append ("  z33 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c33 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("}\n");
		} else if (nphases == 2 && name.contains ("triplex")) {
			buf.append ("object triplex_line_configuration {\n  name \"tcon_" + name + "\";\n");
			seq = GetMatIdx(nphases, 0, 0);
			buf.append ("  z11 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			seq = GetMatIdx(nphases, 1, 0);
			buf.append ("  z12 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  z21 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			seq = GetMatIdx(nphases, 1, 1);
			buf.append ("  z22 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("}\n");
		} else if (nphases == 2) {
			buf.append ("object line_configuration {\n  name \"lcon_" + name + "_AB\";\n");
			seq = GetMatIdx(nphases, 0, 0);
			buf.append ("  z11 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c11 " + String.format("%6g", cMat[seq]) + ";\n");
			seq = GetMatIdx(nphases, 1, 0);
			buf.append ("  z12 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c12 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("  z21 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c21 " + String.format("%6g", cMat[seq]) + ";\n");
			seq = GetMatIdx(nphases, 1, 1);
			buf.append ("  z22 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c22 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("}\n");
			buf.append ("object line_configuration {\n  name \"lcon_" + name + "_BC\";\n");
			seq = GetMatIdx(nphases, 0, 0);
			buf.append ("  z22 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c22 " + String.format("%6g", cMat[seq]) + ";\n");
			seq = GetMatIdx(nphases, 1, 0);
			buf.append ("  z23 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c23 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("  z32 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c32 " + String.format("%6g", cMat[seq]) + ";\n");
			seq = GetMatIdx(nphases, 1, 1);
			buf.append ("  z33 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c33 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("}\n");
			buf.append ("object line_configuration {\n  name \"lcon_" + name + "_AC\";\n");
			seq = GetMatIdx(nphases, 0, 0);
			buf.append ("  z11 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c11 " + String.format("%6g", cMat[seq]) + ";\n");
			seq = GetMatIdx(nphases, 1, 0);
			buf.append ("  z13 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c13 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("  z31 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c31 " + String.format("%6g", cMat[seq]) + ";\n");
			seq = GetMatIdx(nphases, 1, 1);
			buf.append ("  z33 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c33 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("}\n");
		} else if (nphases == 3) {
			buf.append ("object line_configuration {\n  name \"lcon_" + name + "_ABC\";\n");
			seq = GetMatIdx(nphases, 0, 0);
			buf.append ("  z11 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c11 " + String.format("%6g", cMat[seq]) + ";\n");
			seq = GetMatIdx(nphases, 1, 0);
			buf.append ("  z12 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c12 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("  z21 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c21 " + String.format("%6g", cMat[seq]) + ";\n");
			seq = GetMatIdx(nphases, 1, 1);
			buf.append ("  z22 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c22 " + String.format("%6g", cMat[seq]) + ";\n");
			seq = GetMatIdx(nphases, 2, 0);
			buf.append ("  z31 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c31 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("  z13 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c13 " + String.format("%6g", cMat[seq]) + ";\n");
			seq = GetMatIdx(nphases, 2, 1);
			buf.append ("  z32 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c32 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("  z23 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c23 " + String.format("%6g", cMat[seq]) + ";\n");
			seq = GetMatIdx(nphases, 2, 2);
			buf.append ("  z33 " + CFormat (new Complex(rMat[seq], xMat[seq])) + ";\n");
			buf.append ("  c33 " + String.format("%6g", cMat[seq]) + ";\n");
			buf.append ("}\n");
		}
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

	static String FirstPhase (String phs) {
		if (phs.contains ("A")) return "A";
		if (phs.contains ("B")) return "B";
		return "C";
	}

	static String Bus_ShuntPhases (String phs, String conn) {
		if (conn.contains("w") && !phs.contains("N")) return phs + "N";
		if (conn.contains("d") && !phs.contains("D")) return phs + "D";
		return phs;
	}

	static boolean Shunt_Delta (Resource r, Property p) {
		if (r.hasProperty(p)) {
			String arg = r.getProperty(p).getObject().toString();
			int hash = arg.lastIndexOf ("#PhaseShuntConnectionKind.");
			String conn = arg.substring (hash + 26);
			if (conn.contains ("D")) {
				return true;
			}
		}
		return false;
	}

  static String WirePhases (Model mdl, Resource r, Property p1, Property p2) {
    ResIterator it = mdl.listResourcesWithProperty (p1, r);
    if (it.hasNext()) {  // we don't know what order the phases will come
      boolean bA = false;
      boolean bB = false;
      boolean bC = false;
			boolean bSecondary = false;
      while (it.hasNext()) {
        Resource rP = it.nextResource();
        if (rP.hasProperty(p2)) {
          String s = Phase_Kind_String (rP.getProperty(p2).getObject().toString());
          if (s.equals("A")) bA = true;
          if (s.equals("B")) bB = true;
          if (s.equals("C")) bC = true;
					if (s.equals("s1")) bSecondary = true;
					if (s.equals("s2")) bSecondary = true;
        }
      }
      StringBuilder buf = new StringBuilder ("");
      if (bA) buf.append ("A");
      if (bB) buf.append ("B");
      if (bC) buf.append ("C");
			if (bSecondary) buf.append ("S");
      return buf.toString();
    }
    return "ABC";
  }

  static int Count_Phases (String phs) {
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
      return arg.substring (hash + 19);  // D, Y, Z, Yn, Zn, A, I
    }
    return def;
  }

  static String GetPropValue (Model mdl, String uri, String prop) {
    Resource res = mdl.getResource (uri);
    Property p = mdl.getProperty (nsCIM, prop);
    return res.getProperty(p).getString();
  }

  static boolean AccumulateLoads (GldNode nd, String phs, double pL, double qL, double Pv, double Qv,
																	double Pz, double Pi, double Pp, double Qz, double Qi, double Qp) {
		// we have to equally divide the total pL and qL among the actual phases defined in "phs"
		double fa = 0.0, fb = 0.0, fc = 0.0, denom = 0.0;
		if (phs.contains("A")) {
			fa = 1.0;
			denom += 1.0;
		}
		if (phs.contains("B")) {
			fb = 1.0;
			denom += 1.0;
		}
		if (phs.contains("C")) {
			fc = 1.0;
			denom += 1.0;
		}
		if (fa > 0.0) fa /= denom;
		if (fb > 0.0) fb /= denom;
		if (fc > 0.0) fc /= denom;

		// we also have to divide the total pL and qL among constant ZIP components
		double fpz = 0.0, fqz = 0.0, fpi = 0.0, fqi = 0.0, fpp = 0.0, fqp = 0.0;
		denom = Pz + Pi + Pp;
		if (denom > 0.0) {
			fpz = Pz / denom;
			fpi = Pi / denom;
			fpp = Pp / denom;
		} else {
			if (Pv > 0.9 && Pv < 1.1)	{
				fpi = 1.0;
			} else if (Pv > 1.9 && Pv < 2.1) {
				fpz = 1.0;
			} else {
				fpp = 1.0;
			}
		}
		denom = Qz + Qi + Qp;
		if (denom > 0.0) {
			fqz = Qz / denom;
			fqi = Qi / denom;
			fqp = Qp / denom;
		} else {
			if (Qv > 0.9 && Qv < 1.1)	{
				fqi = 1.0;
			} else if (Qv > 1.9 && Qv < 2.1) {
				fqz = 1.0;
			} else {
				fqp = 1.0;
			}
		}

		// now update the node phases and phase loads
		nd.AddPhases(phs);
		nd.pa_z += fa * pL * fpz;
		nd.pb_z += fb * pL * fpz;
		nd.pc_z += fc * pL * fpz;
		nd.qa_z += fa * qL * fqz;
		nd.qb_z += fb * qL * fqz;
		nd.qc_z += fc * qL * fqz;
		nd.pa_i += fa * pL * fpi;
		nd.pb_i += fb * pL * fpi;
		nd.pc_i += fc * pL * fpi;
		nd.qa_i += fa * qL * fqi;
		nd.qb_i += fb * qL * fqi;
		nd.qc_i += fc * qL * fqi;
		nd.pa_p += fa * pL * fpp;
		nd.pb_p += fb * pL * fpp;
		nd.pc_p += fc * pL * fpp;
		nd.qa_p += fa * qL * fqp;
		nd.qb_p += fb * qL * fqp;
		nd.qc_p += fc * qL * fqp;
    return true;
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
          return GLD_Name (CN.getProperty(ptName).getString(), true);
        } else {
          return GLD_Name (CN.getLocalName(), true);
        }
      }
    }
    return "x";
  }

	static String GetGldTransformerConnection(String [] wye, int nwdg) {
		if (nwdg == 3) {
			if (wye[0].equals("I") && wye[1].equals("I") && wye[1].equals("I")) return "SINGLE_PHASE_CENTER_TAPPED";
		}
		if (wye[0].equals("D"))
		{
			if (wye[1].equals("D"))	{
				return "DELTA_DELTA";
			} else if (wye[1].equals("Y")) {
				return "DELTA_GWYE";
			} else if (wye[1].equals("Z")) {
				return "D_Z";
			} else if (wye[1].equals("Yn")) {
				return "DELTA_GWYE";
			} else if (wye[1].equals("Zn")) {
				return "D_Zn";
			} else if (wye[1].equals("A")) {
				return "D_A";
			} else if (wye[1].equals("I")) {
				return "D_I";
			}
		} else if (wye[0].equals("Y")) {
			if (wye[1].equals("D"))	{
				return "Y_D";  // TODO - flip?
			} else if (wye[1].equals("Y")) {
				return "WYE_WYE";
			} else if (wye[1].equals("Z")) {
				return "Y_Z";
			} else if (wye[1].equals("Yn")) {
				return "WYE_WYE";
			} else if (wye[1].equals("Zn")) {
				return "Y_Z";
			} else if (wye[1].equals("A")) {
				return "WYE_WYE";  // TODO - approximately correct
			} else if (wye[1].equals("I")) {
				return "Y_I";
			}
		} else if (wye[0].equals("Z")) {
			if (wye[1].equals("D"))	{
				return "Z_D";
			} else if (wye[1].equals("Y")) {
				return "Z_Y";
			} else if (wye[1].equals("Z")) {
				return "Z_Z";
			} else if (wye[1].equals("Yn")) {
				return "Z_Yn";
			} else if (wye[1].equals("Zn")) {
				return "Z_Zn";
			} else if (wye[1].equals("A")) {
				return "Z_A";
			} else if (wye[1].equals("I")) {
				return "Z_I";
			}
		} else if (wye[0].equals("Yn")) {
			if (wye[1].equals("D"))	{
				return "Yn_D";
			} else if (wye[1].equals("Y")) {
				return "WYE_WYE";
			} else if (wye[1].equals("Z")) {
				return "Yn_Z";
			} else if (wye[1].equals("Yn")) {
				return "WYE_WYE";
			} else if (wye[1].equals("Zn")) {
				return "Yn_Zn";
			} else if (wye[1].equals("A")) {
				return "WYE_WYE";  // TODO - approximately correct
			} else if (wye[1].equals("I")) {
				return "Yn_I";
			}
		} else if (wye[0].equals("Zn")) {
			if (wye[1].equals("D"))	{
				return "Zn_D";
			} else if (wye[1].equals("Y")) {
				return "Zn_Y";
			} else if (wye[1].equals("Z")) {
				return "Zn_Z";
			} else if (wye[1].equals("Yn")) {
				return "Zn_Yn";
			} else if (wye[1].equals("Zn")) {
				return "Zn_Zn";
			} else if (wye[1].equals("A")) {
				return "Zn_A";
			} else if (wye[1].equals("I")) {
				return "Zn_I";
			}
		} else if (wye[0].equals("A")) {
			if (wye[1].equals("D"))	{
				return "A_D";
			} else if (wye[1].equals("Y")) {
				return "WYE_WYE";  // TODO - approximately correct
			} else if (wye[1].equals("Z")) {
				return "A_Z";
			} else if (wye[1].equals("Yn")) {
				return "WYE_WYE";  // TODO - approximately correct
			} else if (wye[1].equals("Zn")) {
				return "A_Zn";
			} else if (wye[1].equals("A")) {
				return "WYE_WYE";  // TODO - approximately correct
			} else if (wye[1].equals("I")) {
				return "A_I";
			}
		} else if (wye[0].equals("I")) {
			if (wye[1].equals("D"))	{
				return "I_D";
			} else if (wye[1].equals("Y")) {
				return "I_Y";
			} else if (wye[1].equals("Z")) {
				return "I_Z";
			} else if (wye[1].equals("Yn")) {
				return "I_Yn";
			} else if (wye[1].equals("Zn")) {
				return "I_Zn";
			} else if (wye[1].equals("A")) {
				return "I_A";
			} else if (wye[1].equals("I")) {
				return "SINGLE_PHASE";
			}
		}
		return "** Unsupported **";  // TODO
	}

  static String GetPowerTransformerData (Model mdl, Resource rXf) {
		Property ptEnd = mdl.getProperty (nsCIM, "TransformerEnd.endNumber");
		Property ptTerm = mdl.getProperty (nsCIM, "TransformerEnd.Terminal");
		Property ptPhs = mdl.getProperty (nsCIM, "ConductingEquipment.phases"); // TODO - not there any longer
		Property ptNode = mdl.getProperty (nsCIM, "Terminal.ConnectivityNode");
		Property ptName = mdl.getProperty (nsCIM, "IdentifiedObject.name");
		// used to collect the PowerTransformerEnds belonging to xf_id
		Property ptXfmr = mdl.getProperty (nsCIM, "PowerTransformerEnd.PowerTransformer");
    Property ptEndRw = mdl.getProperty (nsCIM, "PowerTransformerEnd.r");
    Property ptEndC = mdl.getProperty (nsCIM, "PowerTransformerEnd.connectionKind");
//		Property ptEndK = mdl.getProperty (nsCIM, "PowerTransformerEnd.phaseAngleClock");
		Property ptEndV = mdl.getProperty (nsCIM, "PowerTransformerEnd.ratedU");
    Property ptEndS = mdl.getProperty (nsCIM, "PowerTransformerEnd.ratedS");
		Property ptEndGrnd = mdl.getProperty (nsCIM, "TransformerEnd.grounded");
    Property ptEndRn   = mdl.getProperty (nsCIM, "TransformerEnd.rground");
    Property ptEndXn   = mdl.getProperty (nsCIM, "TransformerEnd.xground");
    Property ptEndN    = mdl.getProperty (nsCIM, "TransformerEnd.endNumber");

		String xfName = mdl.getProperty(rXf,ptName).getString();

    // first count the number of windings
    ResIterator it;
    Resource rEnd;
    int i, nwdg = 0;
    it = mdl.listResourcesWithProperty (ptXfmr, rXf);
    while (it.hasNext()) {
      ++nwdg;
      it.nextResource();
    }
    String bus[] = new String[nwdg];
    String phs[] = new String[nwdg];
		String xfmrPhase; // GridLAB-D requires the same phasing on both sides
		double v[] = new double[nwdg];
		double s[] = new double[nwdg];
		double zb[] = new double[nwdg];
		double rw[] = new double[nwdg];
		double rn[] = new double[nwdg];
		double xn[] = new double[nwdg];
		String wye[] = new String[nwdg];
		Resource rEnds[] = new Resource[nwdg];

		// now pull bus names in correct order, by winding endNumber
		it = mdl.listResourcesWithProperty (ptXfmr, rXf);
		while (it.hasNext()) {  // pulls all the Ends for xf_id
			Resource wdg = it.nextResource();
			i = SafeInt (wdg, ptEnd, 1) - 1;
			Resource trm = wdg.getProperty(ptTerm).getResource();
			phs[i] = Phase_String(SafePhasesX (trm, ptPhs));
			Resource CN = trm.getProperty(ptNode).getResource();
			if (CN.hasProperty(ptName)) {
				bus[i] = GLD_Name (CN.getProperty(ptName).getString(), true);
			} else {
				bus[i] = GLD_Name (CN.getLocalName(), true);
			}
		}

		// now go through the PowerTransformerEnds; we can only deal with two (GridLAB-D limit) or three
    it = mdl.listResourcesWithProperty (ptXfmr, rXf);
    while (it.hasNext()) {
      rEnd = it.nextResource();
      i = SafeInt (rEnd, ptEndN, 1) - 1;
      v[i] = SafeDouble (rEnd, ptEndV, 1.0); // v
      s[i] = SafeDouble (rEnd, ptEndS, 1.0); // va
			zb[i] = v[i] * v[i] / s[i];
			rw[i] = SafeDouble (rEnd, ptEndRw, 0.0) / zb[i];
      rn[i] = SafeDouble (rEnd, ptEndRn, 0.0);
      xn[i] = SafeDouble (rEnd, ptEndXn, 0.0);
      wye[i] = GetWdgConnection (rEnd, ptEndC, "Y");
			rEnds[i] = rEnd; // save to construct the impedance data
    }

		// find the Xhl, Xht, Xlt, and core values from TransformerMeshImpedance, TransformerCoreAdmittance
		Property ptFrom = mdl.getProperty (nsCIM, "TransformerMeshImpedance.FromTransformerEnd");
		Property ptTo = mdl.getProperty (nsCIM, "TransformerMeshImpedance.ToTransformerEnd");
		Property ptMeshX = mdl.getProperty (nsCIM, "TransformerMeshImpedance.x");
		Property ptCoreB = mdl.getProperty (nsCIM, "TransformerCoreAdmittance.b");
		Property ptCoreG = mdl.getProperty (nsCIM, "TransformerCoreAdmittance.g");
		Property ptCoreN = mdl.getProperty (nsCIM, "TransformerCoreAdmittance.TransformerEnd");
		Resource rMesh, rCore, rTo;
		double x, b, g;

    StringBuilder bufX = new StringBuilder ("object transformer_configuration {\n");
		bufX.append ("  name \"xcon_" + xfName + "\";\n");
		bufX.append ("  connect_type " + GetGldTransformerConnection (wye, nwdg) + ";\n");
		bufX.append ("  primary_voltage " + String.format("%6g", v[0]) + ";\n");
		bufX.append ("  secondary_voltage " + String.format("%6g", v[1]) + ";\n");
		bufX.append ("  power_rating " + String.format("%6g", s[0]) + ";\n");
		bufX.append ("  resistance " + String.format("%6g", rw[0] + rw[1]) + ";\n");

		it = mdl.listResourcesWithProperty (ptFrom, rEnds[0]);
		while (it.hasNext()) {
			rMesh = it.nextResource();
			rTo = rMesh.getProperty(ptTo).getResource();
			x = SafeDouble(rMesh, ptMeshX, 1.0) / zb[0];
			if (rTo.equals (rEnds[1]))	{
				bufX.append("  reactance " + String.format("%6g", x) + ";\n");
			}
		}
		for (i = 0; i < nwdg; i++) {
			it = mdl.listResourcesWithProperty(ptCoreN, rEnds[i]);
			while (it.hasNext()) {
				rCore = it.nextResource();
				g = SafeDouble(rCore, ptCoreG, 0.0) * zb[i];
				b = SafeDouble(rCore, ptCoreB, 0.0) * zb[i];
				if (g > 0.0) {
					bufX.append ("  shunt_resistance " + String.format("%6g", 1.0 / g) + ";\n");
				}
				if (b > 0.0) {
					bufX.append ("  shunt_reactance " + String.format("%6g", 1.0 / b) + ";\n");
				}
			}
		}

    if (nwdg > 2) {
			bufX.append("// ***** too many windings for GridLAB-D *****\n");
    }
		bufX.append ("}\n");

		if (phs[1].contains("S"))	{
			xfmrPhase = phs[1];
		} else {
			xfmrPhase = phs[0];
		}

		bufX.append ("object transformer {\n");
		bufX.append ("  name \"xf_" + xfName + "\";\n");
		bufX.append ("  configuration \"xcon_" + xfName + "\";\n");
		bufX.append ("  from \"" + bus[0] + "\";\n");
		bufX.append ("  to \"" + bus[1] + "\";\n");
		bufX.append ("  phases " + xfmrPhase + ";\n");  // no difference between primary and secondary phases
		bufX.append ("}\n");

		for (i = 0; i < nwdg; i++) {
			GldNode nd = mapNodes.get(bus[i]);
			nd.AddPhases (xfmrPhase); // TODO - not right, we can't add "S" to both sides
			nd.nomvln = v[i] / Math.sqrt(3.0);
		}
		return bufX.toString();
  }

	// triggered from PowerTransformers that have RatioTapChangers attached
	static String GetRegulatorData (Model mdl, Resource rXf, String name, String xfGroup, String bus1, String bus2, String phs) {

		boolean bA = false, bB = false, bC = false;
		int iTapA = 0, iTapB = 0, iTapC = 0;
		double ldcRa = 0.0, ldcRb = 0.0, ldcRc = 0.0;
		double ldcXa = 0.0, ldcXb = 0.0, ldcXc = 0.0;
		double CT = 1.0;
		double PT = 1.0;
		double Vband = 2.0, Vreg = 120.0;
		int highStep = 0, lowStep = 0, neutralStep = 0, normalStep = 0;
		double initDelay = 0;
		Property ptName = mdl.getProperty (nsCIM, "IdentifiedObject.name");

		ResIterator itTank = mdl.listResourcesWithProperty (mdl.getProperty (nsCIM, "TransformerTank.PowerTransformer"), rXf);
		while (itTank.hasNext()) {
			Resource rTank = itTank.nextResource();
			ResIterator it = mdl.listResourcesWithProperty (mdl.getProperty (nsCIM, "TransformerTankEnd.TransformerTank"), rTank);  // pulls all the transformer ends
			while (it.hasNext()) {
				Resource wdg = it.nextResource();
				String this_phs = Phase_String(SafePhasesX (wdg, mdl.getProperty (nsCIM, "TransformerTankEnd.phases")));
				ResIterator itRtc = mdl.listResourcesWithProperty(mdl.getProperty (nsCIM, "RatioTapChanger.TransformerEnd"), wdg); // look for a connected regulator
				if (itRtc.hasNext()) {
					Resource rtc = itRtc.nextResource();
					Resource ctl = mdl.getProperty (rtc, mdl.getProperty (nsCIM, "TapChanger.TapChangerControl")).getResource();
					double ldcX = SafeDouble (ctl, mdl.getProperty (nsCIM, "TapChangerControl.lineDropX"), 0.0);
					double ldcR = SafeDouble (ctl, mdl.getProperty (nsCIM, "TapChangerControl.lineDropR"), 0.0);
					Vreg = SafeDouble (ctl, mdl.getProperty (nsCIM, "RegulatingControl.targetValue"), 120.0);
					Vband = SafeDouble (ctl, mdl.getProperty (nsCIM, "RegulatingControl.targetDeadband"), 2.0);
					Vreg *= PT;
					Vband *= PT;
					highStep = SafeInt (rtc, mdl.getProperty(nsCIM, "TapChanger.highStep"), 32);
					lowStep = SafeInt (rtc, mdl.getProperty(nsCIM, "TapChanger.highStep"), 0);
					neutralStep = SafeInt (rtc, mdl.getProperty(nsCIM, "TapChanger.neutralStep"), 16);
					normalStep = SafeInt (rtc, mdl.getProperty(nsCIM, "TapChanger.normalStep"), 16);
					initDelay = SafeDouble (rtc, mdl.getProperty(nsCIM, "TapChanger.initialDelay"), 30);
					double subsDelay = SafeDouble (rtc, mdl.getProperty(nsCIM, "TapChanger.subsequentDelay"), 2);
					double dTap = SafeDouble(rtc, mdl.getProperty (nsCIM, "TapChanger.step"), 1.0);
					int iTap = (int) Math.round((dTap - 1.0) / 0.00625);  // TODO - verify this is an offset from neutralStep
					if (this_phs.contains("A")) {
						bA = true;
						iTapA = iTap;
						ldcXa = ldcX;
						ldcRa = ldcR;
					}
					if (this_phs.contains("B")) {
						bB = true;
						iTapB = iTap;
						ldcXb = ldcX;
						ldcRb = ldcR;
					}
					if (this_phs.contains("C")) {
						bC = true;
						iTapC = iTap;
						ldcXc = ldcX;
						ldcRc = ldcR;
					}
					// look up the asset datasheet
					ResIterator itAsset = mdl.listResourcesWithProperty (mdl.getProperty (nsCIM, "Asset.PowerSystemResources"), rtc);
					while (itAsset.hasNext()) {
						Resource rAsset = itAsset.nextResource();
						if (rAsset.hasProperty(mdl.getProperty (nsCIM, "Asset.AssetInfo"))) {
							Resource rDS = rAsset.getProperty(mdl.getProperty (nsCIM, "Asset.AssetInfo")).getResource();
							PT = SafeDouble (rDS, mdl.getProperty (nsCIM, "TapChangerInfo.ptRatio"), 1.0);
							CT = SafeDouble (rDS, mdl.getProperty (nsCIM, "TapChangerInfo.ctRating"), 1.0);
						}
					}
				}
			}
		}

		StringBuffer buf = new StringBuffer("object regulator_configuration {\n  name \"rcon_" + name + "\";\n");
		if (xfGroup.contains("D") || xfGroup.contains("d"))	{
			buf.append ("  connect_type CLOSED_DELTA;\n");
		} else {
			buf.append ("  connect_type WYE_WYE;\n");
		}
		buf.append ("  band_center " + String.format("%6g", Vreg) + ";\n");
		buf.append ("  band_width " + String.format("%6g", Vband) + ";\n");
		buf.append ("  time_delay " + String.format("%6g", initDelay) + ";\n");
		buf.append ("  raise_taps " + String.format("%d", Math.abs (highStep - neutralStep)) + ";\n");
		buf.append ("  lower_taps " + String.format("%d", Math.abs (neutralStep - lowStep)) + ";\n");
		buf.append ("  current_transducer_ratio " + String.format("%6g", CT) + ";\n");
		buf.append ("  power_transducer_ratio " + String.format("%6g", PT) + ";\n");
		if (bA)	{
			buf.append ("  tap_pos_A " + String.format("%d", iTapA) + ";\n");
			buf.append ("  compensator_r_setting_A " + String.format("%6g", ldcRa) + ";\n");
			buf.append ("  compensator_x_setting_A " + String.format("%6g", ldcXa) + ";\n");
		}
		if (bB)	{
			buf.append ("  tap_pos_B " + String.format("%d", iTapB) + ";\n");
			buf.append ("  compensator_r_setting_B " + String.format("%6g", ldcRb) + ";\n");
			buf.append ("  compensator_x_setting_B " + String.format("%6g", ldcXb) + ";\n");
		}
		if (bC)	{
			buf.append ("  tap_pos_C " + String.format("%d", iTapC) + ";\n");
			buf.append ("  compensator_r_setting_C " + String.format("%6g", ldcRc) + ";\n");
			buf.append ("  compensator_x_setting_C " + String.format("%6g", ldcXc) + ";\n");
		}
		buf.append ("}\n");

		buf.append ("object regulator {\n  name \"reg_" + name + "\";\n");
		buf.append ("  from \"" + bus1 + "\";\n");
		buf.append ("  to \"" + bus2 + "\";\n");
		buf.append ("  phases " + phs + ";\n");
		buf.append ("  configuration \"rcon_" + name + "\";\n");
		buf.append ("}\n");

		return buf.toString();
	}

	// this is limited because only 2 windings are allowed, and phasing must be the same on both sides
	static String GetPowerTransformerTanks (Model mdl, Resource rXf, ResIterator itTank) {
		Property ptName = mdl.getProperty (nsCIM, "IdentifiedObject.name");
		Property ptAssetPSR = mdl.getProperty (nsCIM, "Asset.PowerSystemResources");
		Property ptAssetInf = mdl.getProperty (nsCIM, "Asset.AssetInfo");
		Property ptGroup = mdl.getProperty (nsCIM, "PowerTransformer.vectorGroup");
		Property ptEnd = mdl.getProperty (nsCIM, "TransformerTankEnd.TransformerTank");
		Property ptEndN = mdl.getProperty (nsCIM, "TransformerEnd.endNumber");
		Property ptTerm = mdl.getProperty (nsCIM, "TransformerEnd.Terminal");
		Property ptPhs = mdl.getProperty (nsCIM, "TransformerTankEnd.phases");
		Property ptNode = mdl.getProperty (nsCIM, "Terminal.ConnectivityNode");
		Property ptRtcEnd = mdl.getProperty (nsCIM, "RatioTapChanger.TransformerEnd");

		String xfName = SafeResName (rXf, ptName);
		String xfGroup = mdl.getProperty(rXf,ptGroup).getString();
		String xfCode = "";
		String xfPhase;
		String bus [] = new String[2];
		String phs [] = new String[2];
		boolean bRegulator = false;

		// look for the first two buses, accumulate those phases, accept any datasheet
		while (itTank.hasNext()) {
			Resource rTank = itTank.nextResource();
			ResIterator it = mdl.listResourcesWithProperty (ptEnd, rTank);  // pulls all the transformer ends
			while (it.hasNext()) {
				Resource wdg = it.nextResource();
				int i = SafeInt (wdg, ptEndN, 1) - 1;
				if (i >= 0 && i <= 1)	{
					phs[i] = Phase_String(SafePhasesX (wdg, ptPhs));
					Resource trm = wdg.getProperty(ptTerm).getResource();
					Resource CN = trm.getProperty(ptNode).getResource();
					if (CN.hasProperty(ptName)) {  // buses can't differ for each tank in GridLAB-D
						bus[i] = GLD_Name (CN.getProperty(ptName).getString(), true);
					} else {
						bus[i] = GLD_Name (CN.getLocalName(), true);
					}
				}
				ResIterator itRtc = mdl.listResourcesWithProperty (ptRtcEnd, wdg); // look for a connected regulator
				if (itRtc.hasNext()) bRegulator = true;
			}

			it = mdl.listResourcesWithProperty (ptAssetPSR, rTank); // pulls all datasheets
			if (it.hasNext()) {
				Resource rAsset = it.nextResource();
				if (rAsset.hasProperty(ptAssetInf)) {
					Resource rDS = rAsset.getProperty(ptAssetInf).getResource();
					xfCode = mdl.getProperty(rDS,ptName).getString();
				}
			}
		}

		// figure out the phasing for center-tap secondary transformers
		if (phs[0].contains("s"))	{
			phs[0] = phs[1] + "S";
			xfPhase = phs[0];
		} else if (phs[1].contains("s")) {
			phs[1] = phs[0] + "S";
			xfPhase = phs[1];
		} else if (xfGroup.contains("D"))	{
			phs[0] = phs[0] + "D";
			xfPhase = phs[0];
		} else if (xfGroup.contains("d"))	{
			phs[1] = phs[1] + "D";
			xfPhase = phs[1];
		} else {
			xfPhase = phs[0];
		}
		GldNode nd = mapNodes.get(bus[0]);
		nd.AddPhases(phs[0]);
		nd = mapNodes.get(bus[1]);
		nd.AddPhases(phs[1]);

		StringBuilder buf = new StringBuilder("");
		if (bRegulator)	{
			return GetRegulatorData (mdl, rXf, xfName, xfGroup, bus[0], bus[1], phs[0]);
		} else {
			buf.append ("object transformer {\n");
			buf.append ("  name \"xf_" + xfName + "\";\n");
			buf.append ("  from \"" + bus[0] + "\";\n");
			buf.append ("  to \"" + bus[1] + "\";\n");
			buf.append ("  phases " + xfPhase + ";\n");
			buf.append ("  // " + xfGroup + "\n");
			buf.append ("  configuration \"xcon_" + xfCode + "\";\n}\n");
		}

		return buf.toString();
	}

	// needs to return the spacing and wire/cncable/tscable assignments for this rLine
	static String GetLineSpacing (Model mdl, Resource rLine) {
		StringBuilder buf = new StringBuilder (" spacing=");
		Property ptAssetPSR = mdl.getProperty (nsCIM, "Asset.PowerSystemResources");
		Property ptAssetInf = mdl.getProperty (nsCIM, "Asset.AssetInfo");
		Property ptName = mdl.getProperty (nsCIM, "IdentifiedObject.name");
		ResIterator itAsset = mdl.listResourcesWithProperty (ptAssetPSR, rLine);
		int nconds = 0;
		int nphases = 0;
		boolean bCNcables = false;
		boolean bTScables = false;
		String wsName = "";
		String wireName = "";  // attached to the ACLineSegment (i.e. for all phases)
		while (itAsset.hasNext()) {
			Resource rAsset = itAsset.nextResource();
			if (rAsset.hasProperty(ptAssetInf)) {
				Resource rDS = rAsset.getProperty(ptAssetInf).getResource();
				// TODO: .listRDFTypes() might be more robust
				String s = rDS.as(OntResource.class).getRDFType().toString();
				int hash = s.lastIndexOf ("#");
				String t = s.substring (hash + 1);
				if (t.equals("WireSpacingInfo")) {
					wsName = SafeResName (rDS, ptName);
					buf.append (wsName);
					SpacingCount spc = mapSpacings.get(wsName);
					nconds = spc.getNumConductors();
					nphases = spc.getNumPhases();
				} else if (t.equals("OverheadWireInfo")) {
					wireName = SafeResName (rDS, ptName);
				} else if (t.equals("ConcentricNeutralCableInfo")) {
					bCNcables = true;
					wireName = SafeResName (rDS, ptName);
				} else if (t.equals("TapeShieldCableInfo")) {
					bTScables = true;
					wireName = SafeResName (rDS, ptName);
				}
			}
		}
		if (nconds > 0) { // find all the wires by phase for individual assignments
			Property ptSegment = mdl.getProperty (nsCIM, "ACLineSegmentPhase.ACLineSegment");
			Property ptPhase = mdl.getProperty (nsCIM, "ACLineSegmentPhase.phase");
			ResIterator it = mdl.listResourcesWithProperty (ptSegment, rLine);
			String wA = "", wB = "", wC = "", wN = "", wS1 = "", wS2 = "", wS = "";
			while (it.hasNext()) {  // we don't know what order the phases will come
				Resource rP = it.nextResource();
				if (rP.hasProperty(ptPhase)) {
					String sPhase = Phase_Kind_String (rP.getProperty(ptPhase).getObject().toString());
					itAsset = mdl.listResourcesWithProperty (ptAssetPSR, rP);
					while (itAsset.hasNext()) {
						Resource rAsset = itAsset.nextResource();
						if (rAsset.hasProperty(ptAssetInf)) {
							Resource rDS = rAsset.getProperty(ptAssetInf).getResource();
							String s = rDS.as(OntResource.class).getRDFType().toString();
							int hash = s.lastIndexOf ("#");
							String t = s.substring (hash + 1);
							if (t.equals("ConcentricNeutralCableInfo")) bCNcables = true;
							if (t.equals("TapeShieldCableInfo")) bTScables = true;
							wS = SafeResName(rDS, ptName); // if not "", indicates we found at least one wire/cable assigned to a phase
							if (sPhase.equals("A")) wA = wS;
							if (sPhase.equals("B")) wB = wS;
							if (sPhase.equals("C")) wC = wS;
							if (sPhase.equals("N")) wN = wS;
							if (sPhase.equals("s1")) wS1 = wS;
							if (sPhase.equals("s2")) wS2 = wS;
						}
					}
				}
			}
			if (wS.length() < 1) { // no individual phase conductors were found
				if (bCNcables) {
					buf.append(" CNcables=[");
				} else if (bTScables) {
					buf.append(" TScables=[");
				} else {
					buf.append(" wires=[");
				}
				for (int i = 0; i < nconds; i++) buf.append(wireName + " ");
				buf.append("]");
			} else {
				if (bCNcables) {
					buf.append(" CNcables=[");
				} else if (bTScables) {
					buf.append(" TScables=[");
				} else {
					buf.append(" wires=[");
				}
				if (wA.length() > 0) buf.append (wA + " ");
				if (wB.length() > 0) buf.append (wB + " ");
				if (wC.length() > 0) buf.append (wC + " ");
				if (wS1.length() > 0) buf.append (wS1 + " ");
				if (wS2.length() > 0) buf.append (wS2 + " ");
				if (!(bCNcables || bTScables)) { // write any overhead neutrals
					for (int i = 0; i < (nconds - nphases); i++) buf.append(wN + " ");
				}
				buf.append("]");
				if ((nconds > nphases) && (bCNcables || bTScables)) { // separate underground neutrals are actually bare wires
					buf.append(" wires=[");
					for (int i = 0; i < (nconds - nphases); i++) buf.append(wN + " ");
					buf.append("]");
				}
			}
			return buf.toString();
		}
		return "";
	}

	static String GetWireData (Model mdl, Resource res) {
		StringBuffer buf = new StringBuffer("");

		Property ptGMR = mdl.getProperty (nsCIM, "WireInfo.gmr");
		Property ptWireRadius = mdl.getProperty (nsCIM, "WireInfo.radius");
		Property ptWireDiameter = mdl.getProperty (nsCIM, "WireInfo.diameter");
		Property ptWireCurrent = mdl.getProperty (nsCIM, "WireInfo.ratedCurrent");
		Property ptWireR25 = mdl.getProperty (nsCIM, "WireInfo.rAC25");
		Property ptWireR50 = mdl.getProperty (nsCIM, "WireInfo.rAC50");
		Property ptWireR75 = mdl.getProperty (nsCIM, "WireInfo.rAC75");
		Property ptWireRdc = mdl.getProperty (nsCIM, "WireInfo.rDC20");

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

		buf.append (" gmr=" + String.format("%6g", gmr) + " radius=" + String.format("%6g", radius) +
								" rac=" + String.format("%6g", wireRac) + " rdc=" + String.format("%6g", wireRdc) + " normamps=" + String.format("%6g", normamps) + 
								" Runits=m Radunits=m gmrunits=m");
		return buf.toString();
	}

	static String GetCableData (Model mdl, Resource res) {
		StringBuffer buf = new StringBuffer("");

//		Property ptOverCore = mdl.getProperty (nsCIM, "CableInfo.diameterOverCore"); // redundant
		Property ptOverIns = mdl.getProperty (nsCIM, "CableInfo.diameterOverInsulation");
		Property ptOverJacket = mdl.getProperty (nsCIM, "CableInfo.diameterOverJacket");
		Property ptInsLayer = mdl.getProperty (nsCIM, "WireInfo.insulationThickness");

//		double dCore = SafeDouble (res, ptOverCore, 0.0);
		double dIns = SafeDouble (res, ptOverIns, 0.0);
		double dJacket = SafeDouble (res, ptOverJacket, 0.0);
		double tIns = SafeDouble (res, ptInsLayer, 0.0);
		double dEps = 2.3; // TODO - how to put this into the CIM

		buf.append ("\n~ EpsR=" + String.format("%6g", dEps) + " Ins=" + String.format("%6g", tIns) +
								" DiaIns=" + String.format("%6g", dIns) + " DiaCable=" + String.format("%6g", dJacket));
		return buf.toString();
	}

	static String GetCapControlData (Model mdl, Resource rCap, Resource ctl) {
		Property ptTerm = mdl.getProperty (nsCIM, "RegulatingControl.Terminal");
		Property ptDiscrete = mdl.getProperty (nsCIM, "RegulatingControl.discrete");
		Property ptEnabled = mdl.getProperty (nsCIM, "RegulatingControl.enabled");
		Property ptMode = mdl.getProperty (nsCIM, "RegulatingControl.mode");
		Property ptPhase = mdl.getProperty (nsCIM, "RegulatingControl.monitoredPhase");
		Property ptBW = mdl.getProperty (nsCIM, "RegulatingControl.targetDeadband");
		Property ptVal = mdl.getProperty (nsCIM, "RegulatingControl.targetValue");
		Property ptMult = mdl.getProperty (nsCIM, "RegulatingControl.targetValueUnitMultiplier");
		Property ptName = mdl.getProperty (nsCIM, "IdentifiedObject.name");

		String ctlName = SafeResName (ctl, ptName);
		String capName = SafeResName (rCap, ptName);
		double dBW = SafeDouble(ctl, ptBW, 1.0);
		double dVal = SafeDouble(ctl, ptVal, 120.0);
		double dMult = SafeDouble(ctl, ptMult, 1.0);
		double dOn = dMult * (dVal - 0.5 * dBW);
		double dOff = dMult * (dVal + 0.5 * dBW);
		boolean bDiscrete = SafeBoolean (ctl, ptDiscrete, true);
		boolean bEnabled = SafeBoolean (ctl, ptEnabled, true);  // TODO -check enabled for everything
		String sPhase = Phase_String (SafePhasesX (ctl, ptPhase));
		String sMode = GLDCapMode (SafeRegulatingMode (ctl, ptMode, "voltage"));

		// find the monitored element
		Property ptCondEq = mdl.getProperty (nsCIM, "Terminal.ConductingEquipment");
		Resource rTerm = mdl.getProperty(ctl,ptTerm).getResource();
		Resource rCondEq = mdl.getProperty (rTerm,ptCondEq).getResource();
		String sEqType = GetEquipmentType (rCondEq);
		String sEqName = SafeResName (rCondEq, ptName);

		int nterm = 0;
		String sMatch = SafeResName (rTerm, ptName);
		ResIterator iter = mdl.listResourcesWithProperty (ptCondEq, rCondEq);
		while (iter.hasNext()) {
			++nterm;
			Resource r = iter.nextResource();
			String s = SafeResName (r, ptName);
			if (s.equals(sMatch)) break;
		}

		StringBuffer buf = new StringBuffer("  control " + sMode + ";\n");
		if (sMode.equals("VOLT"))	{
			buf.append ("  voltage_set_low " + String.format("%6g", dOn) + ";\n");
			buf.append ("  voltage_set_high " + String.format("%6g", dOff) + ";\n");
		} else if (sMode.equals("CURRENT"))	{
			buf.append ("  current_set_low " + String.format("%6g", dOn) + ";\n");
			buf.append ("  current_set_high " + String.format("%6g", dOff) + ";\n");
		} else if (sMode.equals("VAR"))	{
			// in GridLAB-D, positive VAR flow is from capacitor into the upstream remote sensing link (opposite of OpenDSS)
//			buf.append ("  VAr_set_low " + String.format("%6g", dOn) + ";\n");
//			buf.append ("  VAr_set_high " + String.format("%6g", dOff) + ";\n");
			buf.append ("  VAr_set_low " + String.format("%6g", dOff) + ";\n");
			buf.append ("  VAr_set_high " + String.format("%6g", dOn) + ";\n");
		}
		if (!sEqType.equals("cap") || !sEqName.equals(capName)) {
			buf.append("  remote_sense \"" + sEqType + "_" + sEqName + "\";\n");
		}
		buf.append ("  pt_phase " + sPhase + ";\n");
		if (sPhase.length() > 1) {
			buf.append("  control_level INDIVIDUAL;");
		} else {
			buf.append("  control_level BANK;");
		}
		return buf.toString();
	}

  static String GetXfmrCode (Model mdl, String id, double smult, double vmult) {  
    Property ptInfo = mdl.getProperty (nsCIM, "TransformerEndInfo.TransformerTankInfo");
		Property ptEndN = mdl.getProperty (nsCIM, "TransformerEndInfo.endNumber");
    Property ptU = mdl.getProperty (nsCIM, "TransformerEndInfo.ratedU");
    Property ptS = mdl.getProperty (nsCIM, "TransformerEndInfo.ratedS");
    Property ptR = mdl.getProperty (nsCIM, "TransformerEndInfo.r");
    Property ptC = mdl.getProperty (nsCIM, "TransformerEndInfo.connectionKind");
		Property ptCk = mdl.getProperty (nsCIM, "TransformerEndInfo.phaseAngleClock");
    Property ptFrom = mdl.getProperty (nsCIM, "ShortCircuitTest.EnergisedEnd");
    Property ptTo = mdl.getProperty (nsCIM, "ShortCircuitTest.GroundedEnds"); // TODO - this is actually a collection
		Property ptZsc = mdl.getProperty (nsCIM, "ShortCircuitTest.leakageImpedance");
    Property ptLL = mdl.getProperty (nsCIM, "ShortCircuitTest.loss");
		Property ptEnd = mdl.getProperty (nsCIM, "NoLoadTest.EnergisedEnd");
		Property ptNLL = mdl.getProperty (nsCIM, "NoLoadTest.loss");
    Property ptImag = mdl.getProperty (nsCIM, "NoLoadTest.excitingCurrent");

    Resource xfRes = mdl.getResource (id);
		String name = SafeResName (xfRes, mdl.getProperty (nsCIM, "IdentifiedObject.name"));

		// count windings and allocate storage
		int nWindings = 0;
		int nPhases = 3;
		ResIterator iter = mdl.listResourcesWithProperty (ptInfo, xfRes);
		while (iter.hasNext()) {
			Resource wdg = iter.nextResource();
			++nWindings;
		}
		double dU[] = new double[nWindings];
		double dS[] = new double[nWindings];
		double dR[] = new double[nWindings];
		String sC[] = new String[nWindings];
		double dNLL = 0.0, dImag = 0.0, dXsc = 0.0, dXhl = 0.0, dXlt = 0.0, dXht = 0.0;

		// load the winding data
    iter = mdl.listResourcesWithProperty (ptInfo, xfRes);
    while (iter.hasNext()) {
      Resource wdg = iter.nextResource();
			int i = wdg.getProperty(ptEndN).getInt() - 1;
      dU[i] = vmult * SafeDouble (wdg, ptU, 1);
      dS[i] = smult * SafeDouble (wdg, ptS, 1);
      dR[i] = SafeDouble (wdg, ptR, 0);
      double Zbase = dU[i] * dU[i] / dS[i];
      dR[i] /= Zbase;
      sC[i] = GetWdgConnection (wdg, ptC, "Y");
      if (sC[i].equals ("I")) {
        nPhases = 1;
      }
			// find the short circuit tests - TODO only for up to 3 windings? Is the first one found always the right one?
      ResIterator iterTest = mdl.listResourcesWithProperty (ptFrom, wdg);
      while (iterTest.hasNext()) {
        Resource test = iterTest.nextResource();
        dXsc = SafeDouble (test, ptZsc, 0.0001) / Zbase;
      }
			// find the first no-load test
			iterTest = mdl.listResourcesWithProperty (ptEnd, wdg);
			while (iterTest.hasNext()) {
				Resource test = iterTest.nextResource();
				dNLL = SafeDouble (test, ptNLL, 0);
				dImag = SafeDouble (test, ptImag, 0);
			}
    }
		double ibase = dS[0] / dU[0];
		if (nPhases > 1)	{
			ibase /= Math.sqrt(3.0);
			for (int i = 0; i < nWindings; i++) dU[i] /= Math.sqrt(3.0);
		}
		dNLL /= dS[0];
		dImag /= ibase;
		String ConnectType = GetGldTransformerConnection (sC, nWindings);

		StringBuilder buf = new StringBuilder ("object transformer_configuration {\n");
		buf.append ("  name \"xcon_" + name + "\";\n");
		buf.append ("  connect_type " + ConnectType + ";\n");
		buf.append ("  primary_voltage " + String.format("%6g", dU[0]) + ";\n");
		buf.append ("  secondary_voltage " + String.format("%6g", dU[1]) + ";\n");
		buf.append ("  power_rating " + String.format("%6g", dS[0]) + ";\n");
		buf.append ("  resistance " + String.format("%6g", dR[0] + dR[1]) + ";\n");
		buf.append ("  reactance " + String.format("%6g", dXsc) + ";\n");
		if (dNLL > 0.0) {
			buf.append ("  shunt_resistance " + String.format("%6g", 1.0 / dNLL) + ";\n");
		}
		if (dImag > 0.0) {
			buf.append ("  shunt_reactance " + String.format("%6g", 1.0 / dImag) + ";\n");
		}
		buf.append("}");
    return buf.toString();
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
		return "";
//    return " normamps=" + String.format("%6g", iMin);
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

	static String GetSequenceLineConfigurations (String name, double sqR1, double sqX1, 
																							 double sqC1, double sqR0, double sqX0, double sqC0) {
		String seqZs = CFormat (new Complex ((sqR0 + 2.0 * sqR1) / 3.0, (sqX0 + 2.0 * sqX1) / 3.0));
		String seqZm = CFormat (new Complex ((sqR0 - sqR1) / 3.0, (sqX0 - sqX1) / 3.0));
		String seqCs = String.format("%6g", (sqC0 + 2.0 * sqC1) / 3.0);
		String seqCm = String.format("%6g", (sqC0 - sqC1) / 3.0);

		StringBuffer buf = new StringBuffer("");

		buf.append("object line_configuration {\n  name \"lcon_" + name + "_ABC\";\n");
		buf.append ("  z11 " + seqZs + ";\n");
		buf.append ("  z12 " + seqZm + ";\n");
		buf.append ("  z13 " + seqZm + ";\n");
		buf.append ("  z21 " + seqZm + ";\n");
		buf.append ("  z22 " + seqZs + ";\n");
		buf.append ("  z23 " + seqZm + ";\n");
		buf.append ("  z31 " + seqZm + ";\n");
		buf.append ("  z32 " + seqZm + ";\n");
		buf.append ("  z33 " + seqZs + ";\n");
		buf.append ("  c11 " + seqCs + ";\n");
		buf.append ("  c12 " + seqCm + ";\n");
		buf.append ("  c13 " + seqCm + ";\n");
		buf.append ("  c21 " + seqCm + ";\n");
		buf.append ("  c22 " + seqCs + ";\n");
		buf.append ("  c23 " + seqCm + ";\n");
		buf.append ("  c31 " + seqCm + ";\n");
		buf.append ("  c32 " + seqCm + ";\n");
		buf.append ("  c33 " + seqCs + ";\n");
		buf.append ("}\n");
		buf.append ("object line_configuration {\n  name \"lcon_" + name + "_AB\";\n");
		buf.append ("  z11 " + seqZs + ";\n");
		buf.append ("  z12 " + seqZm + ";\n");
		buf.append ("  z21 " + seqZm + ";\n");
		buf.append ("  z22 " + seqZs + ";\n");
		buf.append ("  c11 " + seqCs + ";\n");
		buf.append ("  c12 " + seqCm + ";\n");
		buf.append ("  c21 " + seqCm + ";\n");
		buf.append ("  c22 " + seqCs + ";\n");
		buf.append ("}\n");
		buf.append ("object line_configuration {\n  name \"lcon_" + name + "_AC\";\n");
		buf.append ("  z11 " + seqZs + ";\n");
		buf.append ("  z13 " + seqZm + ";\n");
		buf.append ("  z31 " + seqZm + ";\n");
		buf.append ("  z33 " + seqZs + ";\n");
		buf.append ("  c11 " + seqCs + ";\n");
		buf.append ("  c13 " + seqCm + ";\n");
		buf.append ("  c31 " + seqCm + ";\n");
		buf.append ("  c33 " + seqCs + ";\n");
		buf.append ("}\n");
		buf.append ("object line_configuration {\n  name \"lcon_" + name + "_BC\";\n");
		buf.append ("  z22 " + seqZs + ";\n");
		buf.append ("  z23 " + seqZm + ";\n");
		buf.append ("  z32 " + seqZm + ";\n");
		buf.append ("  z33 " + seqZs + ";\n");
		buf.append ("  c22 " + seqCs + ";\n");
		buf.append ("  c23 " + seqCm + ";\n");
		buf.append ("  c32 " + seqCm + ";\n");
		buf.append ("  c33 " + seqCs + ";\n");
		buf.append ("}\n");

		buf.append ("object line_configuration {\n  name \"lcon_" + name + "_A\";\n");
		buf.append ("  z11 " + seqZs + ";\n");
		buf.append ("  c11 " + seqCs + ";\n");
		buf.append ("}\n");
		buf.append ("object line_configuration {\n  name \"lcon_" + name + "_B\";\n");
		buf.append ("  z22 " + seqZs + ";\n");
		buf.append ("  c22 " + seqCs + ";\n");
		buf.append ("}\n");
		buf.append ("object line_configuration {\n  name \"lcon_" + name + "_C\";\n");
		buf.append ("  z33 " + seqZs + ";\n");
		buf.append ("  c33 " + seqCs + ";\n");
		buf.append ("}\n");
		return buf.toString();
	}

	static String GetACLineParameters (Model mdl, String name, Resource r, double len, String phs, PrintWriter out) {
		Property ptR1 = mdl.getProperty (nsCIM, "ACLineSegment.r");
		Property ptR0 = mdl.getProperty (nsCIM, "ACLineSegment.r0");
		Property ptX1 = mdl.getProperty (nsCIM, "ACLineSegment.x");
		Property ptX0 = mdl.getProperty (nsCIM, "ACLineSegment.x0");
		Property ptB1 = mdl.getProperty (nsCIM, "ACLineSegment.bch");
		Property ptB0 = mdl.getProperty (nsCIM, "ACLineSegment.b0ch");

		if (r.hasProperty (ptX1)) {
			double r1 = SafeDouble (r, ptR1, 0); // TODO - verify non-use of len here, and be consistent with OpenDSS importer
			double r0 = SafeDouble (r, ptR0, 0);
			double x1 = SafeDouble (r, ptX1, 0);
			double x0 = SafeDouble (r, ptX0, x1);
			double b0 = SafeDouble (r, ptB0, 0); // EdF writes b0ch but not bch
			double b1 = SafeDouble (r, ptB1, b0); 
			double c0 = 1.0e9 * b0 / 376.991; // TODO - read frequency
			double c1 = 1.0e9 * b1 / 376.991;
			out.println (GetSequenceLineConfigurations (name, r1, x1, c1, r0, x0, c0));
			return "lcon_" + name + "_" + phs;
		}
		return "";
	}

	public static void main (String args[]) throws UnsupportedEncodingException, FileNotFoundException {

    String fProfile = "", fName = "", fOut = "", fBus = "", fEnc = "";
    double freq = 60.0, vmult = 0.001, smult = 0.001;
    int fInFile = 0;
    int fNameSeq = 0;

    if (args.length < 3) {
      System.out.println ("Usage: CDPSM_to_GLM [options] input.xml output_root");
      System.out.println ("       -p={c|a|f|e|g|s|t} // profile; only supports Combined for now");
      System.out.println ("       -e={u|i}           // encoding; UTF-8 or ISO-8859-1");
      System.out.println ("       -f={50|60}         // system frequency");
      System.out.println ("       -v={1|0.001}       // multiplier that converts voltage to V for GridLAB-D");
      System.out.println ("       -s={1000|1|0.001}  // multiplier that converts p,q,s to VA for GridLAB-D");
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
        fOut = args[i] + "_base.glm";
        fBus = args[i] + "_busxy.glm";
      }
      ++i;
    }

    System.out.println (fEnc + " f=" + String.format("%6g", freq) + " v="  + String.format("%6g", vmult) + " s=" + String.format("%6g", smult));

		Model model = ModelFactory.createOntologyModel (OntModelSpec.OWL_DL_MEM);
       
    InputStream in = FileManager.get().open(fName);
    if (in == null) {
      throw new IllegalArgumentException( "File: " + fName + " not found");
    }
        
    PrintWriter out = new PrintWriter (fOut);
    PrintWriter outBus = new PrintWriter (fBus);

    model.read(new InputStreamReader(in, fEnc), baseURI, "RDF/XML");
        
    String qPrefix = "PREFIX r: <" + nsRDF + "> PREFIX c: <" + nsCIM + "> ";
    Query query;
    QueryExecution qexec;
    ResultSet results;
    QuerySolution soln;
    Resource res;
    String id, name, phs, bus1, bus2;
		boolean phs_delta;
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
      name = GldPrefixedNodeName (SafeResName (res, ptName));
      String strPos = GetBusPositionString (model, id);
      if (strPos.length() > 0) {
        outBus.println (name + ", " + strPos);
      } else {
        outBus.println ("// " + name + ", *****");
      }
			mapNodes.put (name, new GldNode(name));
    }
    outBus.println ();
    outBus.close ();
    
    // EnergySource ==> Circuit
    int NumCircuits = 0;
    int NumSources = 0;

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
      name = GLD_Name (soln.get ("?name").toString(), false);
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
        name = GLD_Name (GetPropValue (model, ckt, "IdentifiedObject.name"), false);
				GldNode nd = mapNodes.get(bus1);
				nd.bSwing = true;  
        NumCircuits = 1;
      } else if (name.equals("source")) {
        name = "_" + name;
      }
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
				GldNode nd = mapNodes.get(bus1);
				nd.bSwing = true;  

        name = SafeResName (res, ptName);
//        out.println ("new Circuit." + name + " phases=3 bus1=" + bus1 + " basekv=1");
      }
    }

//    out.println ("// set frequency=" + String.format("%6g", freq));

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
      bus1 = GetBusName (model, id, 1);
      name = SafeResName (res, ptName);
      Resource resUnit = res.getProperty (ptGenRef).getResource();

      double genS = SafeDouble (resUnit, ptGenS, 1.0) * 1000.0;  // assume MW per CPSM
      double genP = SafeDouble (resUnit, ptGenP, 1.0) * 1000.0;
      double genQ = SafeDouble (res, ptGenQ, 0.0) * 1000.0;
      double genQmin = SafeDouble (res, ptGenQmin, 0.44 * genS) * 1000.0 * -1.0;
      double genQmax = SafeDouble (res, ptGenQmax, 0.44 * genS) * 1000.0;
      double genKv = vmult * FindBaseVoltage (res, ptEquip, ptEqBaseV, ptLevBaseV, ptBaseNomV);

//      out.println ("new Generator." + name + " phases=3 bus1=" + bus1 + 
//                   " conn=w kva=" + String.format("%6g", genS) + " kw=" + String.format("%6g", genP) + 
//                   " kvar=" + String.format("%6g", genQ) + " minkvar=" + String.format("%6g", genQmin) + 
//                   " maxkvar=" + String.format("%6g", genQmax) + " kv=" + String.format("%6g", genKv));
    }

    // EnergyConsumer ==> Load
    double total_load_w = 0.0;
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
		Property ptResponse = model.getProperty (nsCIM, "EnergyConsumer.LoadResponse");
		Property ptPv = model.getProperty (nsCIM, "LoadResponseCharacteristic.pVoltageExponent");
		Property ptQv = model.getProperty (nsCIM, "LoadResponseCharacteristic.qVoltageExponent");
		Property ptPz = model.getProperty (nsCIM, "LoadResponseCharacteristic.pConstantImpedance");
		Property ptPi = model.getProperty (nsCIM, "LoadResponseCharacteristic.pConstantCurrent");
		Property ptPp = model.getProperty (nsCIM, "LoadResponseCharacteristic.pConstantPower");
		Property ptQz = model.getProperty (nsCIM, "LoadResponseCharacteristic.qConstantImpedance");
		Property ptQi = model.getProperty (nsCIM, "LoadResponseCharacteristic.qConstantCurrent");
		Property ptQp = model.getProperty (nsCIM, "LoadResponseCharacteristic.qConstantPower");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();

      res = model.getResource (id);
			phs = WirePhases (model, res, ptPhsLoad1, ptPhsLoad2);
			phs_delta = Shunt_Delta (res, ptConnLoad);
      bus1 = GetBusName (model, id, 1);

      name = SafeResName (res, ptName); // not used as a parameter
      double pL = smult * SafeDouble (res, ptP, 1);
      double qL = smult * SafeDouble (res, ptQ, 0);
      total_load_w += pL;
			double Pp = 100, Qp = 100;
			double Pv = 0, Qv = 0, Pz = 0, Qz = 0, Pi = 0, Qi = 0;
			if (res.hasProperty (ptResponse)) {
				Resource rModel = res.getProperty(ptResponse).getResource();
				Pv = SafeDouble (rModel, ptPv, 0);
				Qv = SafeDouble (rModel, ptQv, 0);
				Pz = SafeDouble (rModel, ptPz, 0);
				Pi = SafeDouble (rModel, ptPi, 0);
				Pp = SafeDouble (rModel, ptPp, 0);
				Qz = SafeDouble (rModel, ptQz, 0);
				Qi = SafeDouble (rModel, ptQi, 0);
				Qp = SafeDouble (rModel, ptQp, 0);
			}

			GldNode nd = mapNodes.get(bus1);
			nd.nomvln = FindBaseVoltage (res, ptEquip, ptEqBaseV, ptLevBaseV, ptBaseNomV) / Math.sqrt(3.0);
			if (nd.nomvln < 278.0) nd.bSecondary = true; // TODO - GridLAB-D shouldn't need this
			nd.bDelta = phs_delta;  
			// accumulate P and Q by phase first, and only then update the node phases
			AccumulateLoads (nd, phs, pL, qL, Pv, Qv, Pz, Pi, Pp, Qz, Qi, Qp);  
    }

    // LinearShuntCompensator ==> Capacitor
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:LinearShuntCompensator. " + 
                                 "?s c:IdentifiedObject.name ?name}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptSecB = model.getProperty (nsCIM, "LinearShuntCompensator.bPerSection");
    Property ptSecN = model.getProperty (nsCIM, "LinearShuntCompensator.normalSections");
    Property ptNumSteps = model.getProperty (nsCIM, "ShuntCompensator.maximumSections");
		Property ptPhsShunt1 = model.getProperty (nsCIM, "ShuntCompensatorPhase.ShuntCompensator");
		Property ptPhsShunt2 = model.getProperty (nsCIM, "ShuntCompensatorPhase.phase");
		Property ptConnShunt = model.getProperty (nsCIM, "ShuntCompensator.phaseConnection");
		Property ptAVRDelay = model.getProperty (nsCIM, "ShuntCompensator.aVRDelay");
		Property ptNomU = model.getProperty (nsCIM, "ShuntCompensator.nomU");  // TODO - put in OpenDSS importer
		Property ptCapCtl = model.getProperty (nsCIM, "RegulatingControl.RegulatingCondEq");

		while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = GLD_Name (soln.get ("?name").toString(), false);
      res = model.getResource (id);
			phs = WirePhases (model, res, ptPhsShunt1, ptPhsShunt2);
			phs_delta = Shunt_Delta (res, ptConnShunt);
			bus1 = GetBusName (model, id, 1);
			GldNode nd = mapNodes.get(bus1);
			nd.nomvln = FindBaseVoltage (res, ptEquip, ptEqBaseV, ptLevBaseV, ptBaseNomV) / Math.sqrt(3.0);
			if (nd.nomvln < 278.0) nd.bSecondary = true; // TODO - GridLAB-D shouldn't need this
			nd.AddPhases(phs);

      double cap_b = SafeInt (res, ptNumSteps, 1) * SafeDouble (res, ptSecB, 0.0001);
      double cap_v = SafeDouble (res, ptNomU, 120.0);
      double cap_q = cap_v * cap_v * cap_b;
			cap_q /= phs.length();
			if (phs.length() > 1 && !phs_delta) cap_v /= Math.sqrt(3.0);

			out.println ("object capacitor {");
			out.println ("  name \"cap_" + name + "\";");
			out.println ("  parent \"" + bus1 + "\";");
			if (phs_delta) {
				out.println("  phases " + phs + "D;");
				out.println("  phases_connected " + phs + "D;");
			} else {
				out.println("  phases " + phs + "N;");
				out.println("  phases_connected " + phs + "N;");
			}
			out.println ("  cap_nominal_voltage " + String.format("%6g", cap_v) + ";");
			if (phs.contains("A")) out.println ("  capacitor_A " + String.format("%6g", cap_q) + ";");
			if (phs.contains("B")) out.println ("  capacitor_B " + String.format("%6g", cap_q) + ";");
			if (phs.contains("C")) out.println ("  capacitor_C " + String.format("%6g", cap_q) + ";");

			// see if we have capacitor control settings
			ResIterator itCtl = model.listResourcesWithProperty (ptCapCtl, res);
			if (itCtl.hasNext()) {
				out.println (GetCapControlData (model, res, itCtl.nextResource()));
				double delay = SafeDouble (res, ptAVRDelay, 10.0);
				out.println ("  time_delay " + String.format("%6g", delay) + ";");
			}

			out.println("}");
    }

		// for GridLAB-D, we need to write the transformers first so that we can identify
		// the secondary nodes and carry primary phasing down to them
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
			name = GLD_Name (soln.get ("?name").toString(), false);
			out.println (GetXfmrCode (model, id, smult, vmult));
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
			name = GLD_Name (soln.get ("?name").toString(), false);
			res = model.getResource (id);

			Property ptTank = model.getProperty (nsCIM, "TransformerTank.PowerTransformer");
			ResIterator itTank = model.listResourcesWithProperty (ptTank, res);
			if (itTank.hasNext()) { // write all the tanks to this bank
				out.println (GetPowerTransformerTanks (model, res, itTank));
			} else { // standalone power transformer
				out.println (GetPowerTransformerData (model, res));
			}
		}

    // WireData
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:OverheadWireInfo}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      res = model.getResource (id);
      name = SafeResName (res, ptName);

//      out.println ("new WireData." + name  + GetWireData (model, res));
    }

		// TSData
		out.println ();
		query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:TapeShieldCableInfo}");
		qexec = QueryExecutionFactory.create (query, model);
		results=qexec.execSelect();
		Property ptLap = model.getProperty (nsCIM, "TapeShieldCableInfo.tapeLap");
		Property ptThickness = model.getProperty (nsCIM, "TapeShieldCableInfo.tapeThickness");
		Property ptOverScreen = model.getProperty (nsCIM, "CableInfo.diameterOverScreen");
		while (results.hasNext()) {
			soln = results.next();

			id = soln.get ("?s").toString();
			res = model.getResource (id);
			name = SafeResName (res, ptName);

			double tapeLap = SafeDouble (res, ptLap, 0.0);
			double tapeThickness = SafeDouble (res, ptThickness, 0.0);
			double dScreen = SafeDouble (res, ptOverScreen, 0.0);

//			out.println ("new TSData." + name + GetWireData (model, res) + GetCableData (model, res) +
//									 " DiaShield=" + String.format("%6g", dScreen + 2.0 * tapeThickness) +
//									 " tapeLayer=" + String.format("%6g", tapeThickness) + " tapeLap=" + String.format("%6g", tapeLap));
		}

		// CNData
		out.println ();
		query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:ConcentricNeutralCableInfo}");
		qexec = QueryExecutionFactory.create (query, model);
		results=qexec.execSelect();
		Property ptOverNeutral = model.getProperty (nsCIM, "ConcentricNeutralCableInfo.diameterOverNeutral");
		Property ptStrandCount = model.getProperty (nsCIM, "ConcentricNeutralCableInfo.neutralStrandCount");
		Property ptStrandGmr = model.getProperty (nsCIM, "ConcentricNeutralCableInfo.neutralStrandGmr");
		Property ptStrandRadius = model.getProperty (nsCIM, "ConcentricNeutralCableInfo.neutralStrandRadius");
		Property ptStrandRes = model.getProperty (nsCIM, "ConcentricNeutralCableInfo.neutralStrandRDC20");
		while (results.hasNext()) {
			soln = results.next();

			id = soln.get ("?s").toString();
			res = model.getResource (id);
			name = SafeResName (res, ptName);

			double cnDia = SafeDouble (res, ptOverNeutral, 0.0);
			int cnCount = SafeInt (res, ptStrandCount, 0);
			double cnGmr = SafeDouble (res, ptStrandGmr, 0.0);
			double cnRadius = SafeDouble (res, ptStrandRadius, 0.0);
			double cnRes = SafeDouble (res, ptStrandRes, 0.0);

//			out.println ("new CNData." + name + GetWireData (model, res) + GetCableData (model, res) +
//									 " k=" + Integer.toString(cnCount) + " GmrStrand=" + String.format("%6g", cnGmr) +
//									 " DiaStrand=" + String.format("%6g", 2 * cnRadius) + " Rstrand=" + String.format("%6g", cnRes));
		}

    // LineSpacings (LineGeometries were exported as LineSpacings and individual wire assignments
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:WireSpacingInfo. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptWireX = model.getProperty (nsCIM, "WirePosition.xCoord");
    Property ptWireY = model.getProperty (nsCIM, "WirePosition.yCoord");
    Property ptWireP = model.getProperty (nsCIM, "WirePosition.phase");
		Property ptWireS = model.getProperty (nsCIM, "WirePosition.WireSpacingInfo");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = GLD_Name (soln.get ("?name").toString(), false);
      res = model.getResource (id);

      int nconds=0;
			int nphases=0;
      double wireXa=0, wireXb=0, wireXc=0, wireXn=0, wireXs1=0, wireXs2=0;
			double wireYa=0, wireYb=0, wireYc=0, wireYn=0, wireYs1=0, wireYs2=0;
			boolean wireA = false;
			boolean wireB = false;
			boolean wireC = false;
			boolean wireN = false;
			boolean wireS1 = false;
			boolean wireS2 = false;
      ResIterator wIter = model.listResourcesWithProperty (ptWireS, res);
      while (wIter.hasNext()) {
        Resource wa = wIter.nextResource();
				++nconds;
				phs = Phase_Kind_String (wa.getProperty(ptWireP).getObject().toString()); // TODO - protect
				if (phs.equals("A")) {
					wireXa = SafeDouble (wa, ptWireX, 0);
					wireYa = SafeDouble (wa, ptWireY, 0);
					wireA = true;
					++nphases;
				}
				if (phs.equals("B")) {
					wireXb = SafeDouble (wa, ptWireX, 0);
					wireYb = SafeDouble (wa, ptWireY, 0);
					wireB = true;
					++nphases;
				}
				if (phs.equals("C")) {
					wireXc = SafeDouble (wa, ptWireX, 0);
					wireYc = SafeDouble (wa, ptWireY, 0);
					wireC = true;
					++nphases;
				}
				if (phs.equals("N")) {
					wireXn = SafeDouble (wa, ptWireX, 0);
					wireYn = SafeDouble (wa, ptWireY, 0);
					wireN = true;
				}
				if (phs.equals("s1")) {
					wireXs1 = SafeDouble (wa, ptWireX, 0);
					wireYs1 = SafeDouble (wa, ptWireY, 0);
					wireS1 = true;
					++nphases;
				}
				if (phs.equals("s2")) {
					wireXs2 = SafeDouble (wa, ptWireX, 0);
					wireYs2 = SafeDouble (wa, ptWireY, 0);
					wireS2 = true;
					++nphases;
				}
      }

      if (nconds > 0 && nphases > 0) {
				mapSpacings.put (name, new SpacingCount(nconds, nphases)); // keep track for wire assignments below
//        out.println ("new LineSpacing." + name + " nconds=" + Integer.toString(nconds) +
//										 " nphases=" + Integer.toString(nphases) + " units=m");
				int icond = 0;
				if (wireA)	{
//					out.println ("~ cond=" + Integer.toString(++icond) + 
//											 " x=" + String.format("%6g", wireXa) + " h=" + String.format("%6g", wireYa));
				}
				if (wireB)	{
//					out.println ("~ cond=" + Integer.toString(++icond) + 
//											 " x=" + String.format("%6g", wireXb) + " h=" + String.format("%6g", wireYb));
				}
				if (wireC)	{
//					out.println ("~ cond=" + Integer.toString(++icond) + 
//											 " x=" + String.format("%6g", wireXc) + " h=" + String.format("%6g", wireYc));
				}
				if (wireS1)	{
//					out.println ("~ cond=" + Integer.toString(++icond) + 
//											 " x=" + String.format("%6g", wireXs1) + " h=" + String.format("%6g", wireYs1));
				}
				if (wireS2)	{
//					out.println ("~ cond=" + Integer.toString(++icond) + 
//											 " x=" + String.format("%6g", wireXs2) + " h=" + String.format("%6g", wireYs2));
				}
				if (wireN)	{
//					out.println ("~ cond=" + Integer.toString(++icond) + 
//											 " x=" + String.format("%6g", wireXn) + " h=" + String.format("%6g", wireYn));
				}
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
      name = GLD_Name (soln.get ("?name").toString(), false);
      res = model.getResource (id);
      Property ptCount = model.getProperty (nsCIM, "PerLengthPhaseImpedance.conductorCount");
      if (res.hasProperty (ptCount)) {
        out.println (GetImpedanceMatrix (model, name, ptCount, res));
      }
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
      name = GLD_Name (soln.get ("?name").toString(), false);
      res = model.getResource (id);

			double len = 1609.344; // want ohms/mile and nF/mile

      double sqR1 = len * SafeDouble (res, ptSeqR1, 0);
      double sqR0 = len * SafeDouble (res, ptSeqR0, 0);
      double sqX1 = len * SafeDouble (res, ptSeqX1, 0);
      double sqX0 = len * SafeDouble (res, ptSeqX0, 0);
			double sqC1 = len * SafeDouble (res, ptSeqB1, 0) * 1.0e9 / 377.0;  // TODO: adjustable frequency
			double sqC0 = len * SafeDouble (res, ptSeqB0, 0) * 1.0e9 / 377.0;
      if (sqR0 <= 0) {
        sqR0 = sqR1;
      }
      if (sqX0 <= 0) {
        sqX0 = sqX1;
      }
			out.println (GetSequenceLineConfigurations (name, sqR1, sqX1, sqC1, sqR0, sqX0, sqC0));
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
        name = GLD_ID (id);
      } else {
        name = GLD_Name (soln.get ("?name").toString(), false);
      }
      res = model.getResource (id);
      String len = soln.get ("?len").toString();
      phs = WirePhases (model, res, ptPhsLine1, ptPhsLine2);
      bus1 = GetBusName (model, id, 1);
      bus2 = GetBusName (model, id, 2);
      double dLen = 3.28084 * SafeDouble (res, ptLineLen, 1.0);

			GldNode nd1 = mapNodes.get(bus1);
			nd1.nomvln = FindBaseVoltage (res, ptEquip, ptEqBaseV, ptLevBaseV, ptBaseNomV) / Math.sqrt(3.0);
			GldNode nd2 = mapNodes.get(bus2);
			nd2.nomvln = nd1.nomvln;

			if (phs.contains("S")) { // look for the primary phase at either end of this triplex line segment
				if (nd1.phases.length() > 0) phs = nd1.phases + phs;
				if (nd2.phases.length() > 0) phs = nd2.phases + phs;
			}
			nd1.AddPhases(phs);
			nd2.AddPhases(phs);

			String zPhase = SafeResourceLookup (model, ptName, res, ptPhsZ, "");
			String zParms = GetACLineParameters (model, name, res, dLen, phs, out);
			String zSpace = GetLineSpacing (model, res);
			String linecode = "";

			if (nd1.bSecondary)	{
				out.println("object triplex_line {\n  name \"tpx_" + name + "\";");
			} else {
				out.println("object overhead_line {\n  name \"line_" + name + "\";");
			}
			out.println ("  phases " + phs + ";");
			out.println ("  from \"" + bus1 + "\";");
			out.println ("  to \"" + bus2 + "\";");
			out.println ("  length " + String.format("%6g", dLen) + ";");

      if (zPhase.length() > 0) {
				if (nd1.bSecondary)	{
					out.println("  configuration \"tcon_" + zPhase + "\";");
				} else {
					out.println("  configuration \"lcon_" + zPhase + "_" + phs + "\";");
				}
      } else if (zSpace.length() > 0) {
//        linecode = zSpace;
      } else if (zParms.length() > 0) {
				out.println("  configuration \"" + zParms + "\";");
      }
//      String zAmps = "";
//      if (zParms.length () > 0) {
//        zAmps = FindConductorAmps (model, res, ptDataSheet, ptAmps);
//				out.println (zAmps);
//      }
			out.println ("}");
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
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = GLD_Name (soln.get ("?name").toString(), false);
      res = model.getResource (id);
      phs = WirePhases (model, res, ptPhsSwt1, ptPhsSwt2);
      String open = soln.get ("?open").toString();

      bus1 = GetBusName (model, id, 1);
      bus2 = GetBusName (model, id, 2);

			GldNode nd1 = mapNodes.get(bus1);
			nd1.nomvln = FindBaseVoltage (res, ptEquip, ptEqBaseV, ptLevBaseV, ptBaseNomV) / Math.sqrt(3.0);
			nd1.AddPhases(phs);
			if (nd1.nomvln < 278.0) nd1.bSecondary = true; // TODO - GridLAB-D shouldn't need this
			GldNode nd2 = mapNodes.get(bus2);
			nd2.nomvln = nd1.nomvln;
			nd2.AddPhases(phs);
			if (nd2.nomvln < 278.0) nd2.bSecondary = true; // TODO - GridLAB-D shouldn't need this

			out.println ("object switch {\n  name \"swt_" + name + "\";");
			out.println ("  phases " + phs + ";");
			out.println ("  from \"" + bus1 + "\";");
			out.println ("  to \"" + bus2 + "\";");
      if (open.equals("false")) {
        out.println ("  status CLOSED;");
      } else {
				out.println ("  status OPEN;");
      }
			out.println ("}");
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
      name = GLD_Name (soln.get ("?name").toString(), false);
      res = model.getResource (id);
      phs = WirePhases (model, res, ptPhsSwt1, ptPhsSwt2);
      String open = soln.get ("?open").toString();

      bus1 = GetBusName (model, id, 1);
      bus2 = GetBusName (model, id, 2);

			GldNode nd1 = mapNodes.get(bus1);
			nd1.nomvln = FindBaseVoltage (res, ptEquip, ptEqBaseV, ptLevBaseV, ptBaseNomV) / Math.sqrt(3.0);
			nd1.AddPhases(phs);
			if (nd1.nomvln < 278.0) nd1.bSecondary = true; // TODO - GridLAB-D shouldn't need this
			GldNode nd2 = mapNodes.get(bus2);
			nd2.nomvln = nd1.nomvln;
			nd2.AddPhases(phs);
			if (nd2.nomvln < 278.0) nd2.bSecondary = true; // TODO - GridLAB-D shouldn't need this

//      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
//                          + " switch=y // CIM Fuse");
      if (open.equals("false")) {
//        out.println ("  close Line." + name + " 1");
      } else {
//        out.println ("  open Line." + name + " 1");
      }
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
      name = GLD_Name (soln.get ("?name").toString(), false);
      res = model.getResource (id);
      phs = WirePhases (model, res, ptPhsSwt1, ptPhsSwt2);
      String open = SafeProperty (res, ptOpen, "false");

      bus1 = GetBusName (model, id, 1);
      bus2 = GetBusName (model, id, 2);

			GldNode nd1 = mapNodes.get(bus1);
			nd1.nomvln = FindBaseVoltage (res, ptEquip, ptEqBaseV, ptLevBaseV, ptBaseNomV) / Math.sqrt(3.0);
			nd1.AddPhases(phs);
			if (nd1.nomvln < 278.0) nd1.bSecondary = true; // TODO - GridLAB-D shouldn't need this
			GldNode nd2 = mapNodes.get(bus2);
			nd2.nomvln = nd1.nomvln;
			nd2.AddPhases(phs);
			if (nd2.nomvln < 278.0) nd2.bSecondary = true; // TODO - GridLAB-D shouldn't need this

//      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
//                          + " switch=y // CIM Breaker");
      if (open.equals("false")) {
//        out.println ("  close Line." + name + " 1");
      } else {
//        out.println ("  open Line." + name + " 1");
      }
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
      name = GLD_Name (soln.get ("?name").toString(), false);
      res = model.getResource (id);
      phs = WirePhases (model, res, ptPhsSwt1, ptPhsSwt2);
      String open = SafeProperty (res, ptOpen, "false");

      bus1 = GetBusName (model, id, 1);
      bus2 = GetBusName (model, id, 2);

			GldNode nd1 = mapNodes.get(bus1);
			nd1.nomvln = FindBaseVoltage (res, ptEquip, ptEqBaseV, ptLevBaseV, ptBaseNomV) / Math.sqrt(3.0);
			nd1.AddPhases(phs);
			if (nd1.nomvln < 278.0) nd1.bSecondary = true; // TODO - GridLAB-D shouldn't need this
			GldNode nd2 = mapNodes.get(bus2);
			nd2.nomvln = nd1.nomvln;
			nd2.AddPhases(phs);
			if (nd2.nomvln < 278.0) nd2.bSecondary = true; // TODO - GridLAB-D shouldn't need this

//      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
//                          + " switch=y // CIM Disconnector");
      if (open.equals("false")) {
//        out.println ("  close Line." + name + " 1");
      } else {
//        out.println ("  open Line." + name + " 1");
      }
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
//      out.println ("// new Junction." + name);
    }
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:BusbarSection}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      res = model.getResource (id);
      name = SafeResName (res, ptName);
//      out.println ("// new BusbarSection." + name);
    }
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:Bay}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      res = model.getResource (id);
      name = SafeResName (res, ptName);
//      out.println ("// new Bay." + name);
    }

		// write the nodes and loads; by now, all should have phases and nominal voltage
		for (HashMap.Entry<String,GldNode> pair : mapNodes.entrySet()) {
			GldNode nd = pair.getValue();
			if (nd.HasLoad())	 {
				Complex va = new Complex(nd.nomvln);
				Complex vmagsq = new Complex(nd.nomvln * nd.nomvln);
				if (nd.bSecondary) {
					out.println("object triplex_node {");
					if (nd.bSwing) out.println ("  bustype SWING;");
					out.println ("  name \"" + nd.name + "\";");
					out.println ("  phases " + nd.GetPhases() + ";");
					out.println ("  nominal_voltage " + String.format("%6g", nd.nomvln) + ";");
					if (nd.p1_p > 0.0 || nd.q1_p != 0.0)	{
						out.println ("  power_1 " + CFormat(new Complex(nd.p1_p, nd.q1_p)) + ";");
					}
					if (nd.p2_p > 0.0 || nd.q2_p != 0.0)	{
						out.println ("  power_2 " + CFormat(new Complex(nd.p2_p, nd.q2_p)) + ";");
					}
					if (nd.p1_z > 0.0 || nd.q1_z != 0.0) {
						Complex s = new Complex(nd.p1_z, nd.q1_z);
						Complex z = vmagsq.divide(s.conjugate());
						out.println ("  impedance_1 " + CFormat(z) + ";");
					}
					if (nd.p2_z > 0.0 || nd.q2_z != 0.0) {
						Complex s = new Complex(nd.p2_z, nd.q2_z);
						Complex z = vmagsq.divide(s.conjugate());
						out.println ("  impedance_2 " + CFormat(z) + ";");
					}
					if (nd.p1_i > 0.0 || nd.q1_i != 0.0) {
						Complex s = new Complex(nd.p1_i, nd.q1_i);
						Complex amps = s.divide(va).conjugate();
						out.println ("  current_1 " + CFormat(amps) + ";");
					}
					if (nd.p2_i > 0.0 || nd.q2_i != 0.0) {
						Complex s = new Complex(nd.p2_i, nd.q2_i);
						Complex amps = s.divide(va).conjugate(); // TODO - what should be the angle?
						out.println ("  current_2 " + CFormat(amps) + ";");
					}
					out.println("}");
				} else {
					out.println("object load {");
					if (nd.bSwing) out.println ("  bustype SWING;");
					out.println ("  name \"" + nd.name + "\";");
					out.println ("  phases " + nd.GetPhases() + ";");
					out.println ("  nominal_voltage " + String.format("%6g", nd.nomvln) + ";");
					if (nd.pa_p > 0.0 || nd.qa_p != 0.0)	{
						out.println ("  constant_power_A " + CFormat(new Complex(nd.pa_p, nd.qa_p)) + ";");
					}
					if (nd.pb_p > 0.0 || nd.qb_p != 0.0)	{
						out.println ("  constant_power_B " + CFormat(new Complex(nd.pb_p, nd.qb_p)) + ";");
					}
					if (nd.pc_p > 0.0 || nd.qc_p != 0.0)	{
						out.println ("  constant_power_C " + CFormat(new Complex(nd.pc_p, nd.qc_p)) + ";");
					}
					if (nd.pa_z > 0.0 || nd.qa_z != 0.0) {
						Complex s = new Complex(nd.pa_z, nd.qa_z);
						Complex z = vmagsq.divide(s.conjugate());
						out.println ("  constant_impedance_A " + CFormat(z) + ";");
					}
					if (nd.pb_z > 0.0 || nd.qb_z != 0.0) {
						Complex s = new Complex(nd.pb_z, nd.qb_z);
						Complex z = vmagsq.divide(s.conjugate());
						out.println ("  constant_impedance_B " + CFormat(z) + ";");
					}
					if (nd.pc_z > 0.0 || nd.qc_z != 0.0) {
						Complex s = new Complex(nd.pc_z, nd.qc_z);
						Complex z = vmagsq.divide(s.conjugate());
						out.println ("  constant_impedance_C " + CFormat(z) + ";");
					}
					if (nd.pa_i > 0.0 || nd.qa_i != 0.0) {
						Complex s = new Complex(nd.pa_i, nd.qa_i);
						Complex amps = s.divide(va).conjugate();
						out.println ("  constant_current_A " + CFormat(amps) + ";");
					}
					if (nd.pb_i > 0.0 || nd.qb_i != 0.0) {
						Complex s = new Complex(nd.pb_i, nd.qb_i);
						Complex amps = s.divide(va.multiply(neg120)).conjugate();
						out.println ("  constant_current_B " + CFormat(amps) + ";");
					}
					if (nd.pc_i > 0.0 || nd.qc_i != 0.0) {
						Complex s = new Complex(nd.pc_i, nd.qc_i);
						Complex amps = s.divide(va.multiply(pos120)).conjugate();
						out.println ("  constant_current_C " + CFormat(amps) + ";");
					}
					out.println("}");
				}
			} else {
				if (nd.bSecondary) {
					out.println("object triplex_node {");
				} else {
					out.println("object node {");
				}
				if (nd.bSwing) out.println ("  bustype SWING;");
				out.println ("  name \"" + nd.name + "\";");
				out.println ("  phases " + nd.GetPhases() + ";");
				out.println ("  nominal_voltage " + String.format("%6g", nd.nomvln) + ";");
				out.println ("}");
			}
		}
		out.println ();
		out.println ("// total load = " + String.format("%6g", total_load_w) + " W");

		out.println ("// buscoords " + fBus);
    out.close ();

//		for (HashMap.Entry<String,SpacingCount> pair : mapSpacings.entrySet()) {
//			System.out.printf ("%s ==> %d, %d\n", pair.getKey(), pair.getValue().getNumConductors(), pair.getValue().getNumPhases());
//		}
	}
}

