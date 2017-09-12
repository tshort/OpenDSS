###  # -*- coding: iso-8859-15 -*-
#This is my main code.
__author__ = 'CelsoRocha'

import win32com.client
from win32com.client import makepy
from pylab import *
import os
import csv


class DSS(object): #Classe DSS

    #------------------------------------------------------------------------------------------------------------------#
    def __init__(self, dssFileName):

        # Create a new instance of the DSS
        sys.argv = ["makepy", "OpenDSSEngine.DSS"]
        makepy.main()
        self.dssObj = win32com.client.Dispatch("OpenDSSEngine.DSS")

        # Start the DSS
        if self.dssObj.Start(0) == False:
            print "DSS Failed to Start"
        else:
            #Assign a variable to each of the interfaces for easier access
            self.dssText = self.dssObj.Text
            self.dssCircuit = self.dssObj.ActiveCircuit
            self.dssSolution = self.dssCircuit.Solution
            self.dssCktElement = self.dssCircuit.ActiveCktElement
            self.dssBus = self.dssCircuit.ActiveBus
            self.dssTransformer = self.dssCircuit.Transformers

            # Always a good idea to clear the DSS when loading a new circuit
            self.dssObj.ClearAll()

            # Load the given circuit master file into OpenDSS
            self.dssText.Command = "compile " + dssFileName

            #OpenDSS folder
            self.OpenDSS_folder_path = os.path.dirname(dssFileName)
            self.results_path = self.OpenDSS_folder_path + "/CorrectedLoadShape/results"
            self.dssText.Command = "set DataPath=" + self.results_path

            loadmult = [1,1.15,1.3]
            pv_penetracao = [0,0.1,0.2,0.3]


            for i in range(len(loadmult)):
                for j in range(len(pv_penetracao)):
                    self.dssText.Command = "set case=" + "LM_" +str(loadmult[i]) + "_PEN_" + str(pv_penetracao[j])

                    self.ajuste_param(loadmult[i],pv_penetracao[j])
                    self.dssText.Command = "get loadmult"
                    print self.dssText.Result
                    self.dssText.Command = "? PVSystem.PV_1.Pmpp"
                    print self.dssText.Result


                    # Solve settings
                    self.solve_settings()
                    # self.dssSolution.Number = 2 * self.dssSolution.Number #Resolve dois dias (esperar controles entrarem em regime)
                    self.dssSolution.Solve()  #Solve two days


                    self.dssText.Command = "export monitors Substationtap"
                    self.dssText.Command = "export monitors Reg2a"
                    self.dssText.Command = "export monitors Reg3a"
                    self.dssText.Command = "export monitors Reg4a"
                    self.dssText.Command = "export monitors Reg4b"
                    self.dssText.Command = "export monitors Reg3c"
                    self.dssText.Command = "export monitors Reg4c"
                    self.dssText.Command = "export monitors medidor_trafo_conexao_tensao"
                    self.dssText.Command = "export monitors medidor_trafo_conexao_potencia"
                    self.dssText.Command = "export monitors medidor_PV1"

                    self.dssText.Command = "export monitors SubstationtapV"
                    self.dssText.Command = "export monitors Reg2aV"
                    self.dssText.Command = "export monitors Reg3aV"
                    self.dssText.Command = "export monitors Reg4aV"
                    self.dssText.Command = "export monitors Reg4bV"
                    self.dssText.Command = "export monitors Reg3cV"
                    self.dssText.Command = "export monitors Reg4cV"







     #------------------------------------------------------------------------------------------------------------------#

    def solve_settings(self):

        self.dssText.Command = "set maxcontroliter=200"
        self.dssText.Command = "set maxiterations=100"
        self.dssText.Command = "set mode = daily"
        self.dssText.Command = "set controlmode =time"
        self.dssText.Command = "set stepsize =1s"
        #self.dssSolution.ControlMode = dssTime
        self.dssText.Command = "set number=172800"

    def ajuste_param(self,LM, PV_pen):

        self.dssSolution.LoadMult = LM
        OrigTotalLoad = 3490
        if PV_pen == 0:
            self.dssText.Command = "PVSystem.PV_1.enabled = False"
            self.dssText.Command = "? PVSystem.PV_1.enabled"
            print self.dssText.Result
        else:
            self.dssText.Command = "PVSystem.PV_1.enabled = True"
            #self.dssText.Command = "PVSystem.PV_1.kVA=" + str(OrigTotalLoad*PV_pen)  O OpenDSS
            self.dssText.Command = "PVSystem.PV_1.Pmpp=" +  str(OrigTotalLoad*LM*PV_pen)



    #------------------------------------------------------------------------------------------------------------------#

if __name__ == '__main__':
    d = DSS(r"C:\Users\User\Desktop\TCC\IEEE123Barras\IEEE123Master.dss")




