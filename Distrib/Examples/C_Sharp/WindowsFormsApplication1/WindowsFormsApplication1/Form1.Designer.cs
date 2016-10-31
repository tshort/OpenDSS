namespace WindowsFormsApplication1
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.StartButton = new System.Windows.Forms.Button();
            this.compilebtn = new System.Windows.Forms.Button();
            this.FileNameBox = new System.Windows.Forms.TextBox();
            this.solvebtn = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.ShowBtn = new System.Windows.Forms.Button();
            this.Reg4Adjust = new System.Windows.Forms.Button();
            this.ProfilePLot = new System.Windows.Forms.Button();
            this.label2 = new System.Windows.Forms.Label();
            this.richTextBox1 = new System.Windows.Forms.RichTextBox();
            this.button1 = new System.Windows.Forms.Button();
            this.ResultBox = new System.Windows.Forms.RichTextBox();
            this.button2 = new System.Windows.Forms.Button();
            this.SetVregBtn = new System.Windows.Forms.Button();
            this.VregText = new System.Windows.Forms.TextBox();
            this.button3 = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // StartButton
            // 
            this.StartButton.Location = new System.Drawing.Point(12, 12);
            this.StartButton.Name = "StartButton";
            this.StartButton.Size = new System.Drawing.Size(180, 36);
            this.StartButton.TabIndex = 0;
            this.StartButton.Text = "Start &OpenDSS";
            this.StartButton.UseVisualStyleBackColor = true;
            this.StartButton.Click += new System.EventHandler(this.StartButton_Click);
            // 
            // compilebtn
            // 
            this.compilebtn.Location = new System.Drawing.Point(12, 63);
            this.compilebtn.Name = "compilebtn";
            this.compilebtn.Size = new System.Drawing.Size(180, 39);
            this.compilebtn.TabIndex = 1;
            this.compilebtn.Text = "&Compile File";
            this.compilebtn.UseVisualStyleBackColor = true;
            this.compilebtn.Click += new System.EventHandler(this.compilebtn_Click);
            // 
            // FileNameBox
            // 
            this.FileNameBox.ImeMode = System.Windows.Forms.ImeMode.Off;
            this.FileNameBox.Location = new System.Drawing.Point(207, 73);
            this.FileNameBox.Name = "FileNameBox";
            this.FileNameBox.Size = new System.Drawing.Size(708, 20);
            this.FileNameBox.TabIndex = 2;
            this.FileNameBox.Text = "C:\\OpenDSS\\Distrib\\IEEETestCases\\123Bus\\IEEE123Master.DSS";
            // 
            // solvebtn
            // 
            this.solvebtn.Location = new System.Drawing.Point(12, 108);
            this.solvebtn.Name = "solvebtn";
            this.solvebtn.Size = new System.Drawing.Size(180, 36);
            this.solvebtn.TabIndex = 3;
            this.solvebtn.Text = "&Solve";
            this.solvebtn.UseVisualStyleBackColor = true;
            this.solvebtn.Click += new System.EventHandler(this.solvebtn_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(204, 57);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(57, 13);
            this.label1.TabIndex = 4;
            this.label1.Text = "File Name:";
            // 
            // ShowBtn
            // 
            this.ShowBtn.Location = new System.Drawing.Point(12, 159);
            this.ShowBtn.Name = "ShowBtn";
            this.ShowBtn.Size = new System.Drawing.Size(180, 41);
            this.ShowBtn.TabIndex = 5;
            this.ShowBtn.Text = "Show &Results";
            this.ShowBtn.UseVisualStyleBackColor = true;
            this.ShowBtn.Click += new System.EventHandler(this.ShowBtn_Click);
            // 
            // Reg4Adjust
            // 
            this.Reg4Adjust.Location = new System.Drawing.Point(623, 120);
            this.Reg4Adjust.Name = "Reg4Adjust";
            this.Reg4Adjust.Size = new System.Drawing.Size(161, 53);
            this.Reg4Adjust.TabIndex = 6;
            this.Reg4Adjust.Text = "Adjust Reg4 and Solve";
            this.Reg4Adjust.UseVisualStyleBackColor = true;
            this.Reg4Adjust.Click += new System.EventHandler(this.Reg4Adjust_Click);
            // 
            // ProfilePLot
            // 
            this.ProfilePLot.Location = new System.Drawing.Point(623, 248);
            this.ProfilePLot.Name = "ProfilePLot";
            this.ProfilePLot.Size = new System.Drawing.Size(161, 48);
            this.ProfilePLot.TabIndex = 7;
            this.ProfilePLot.Text = "Plot Pro&file";
            this.ProfilePLot.UseVisualStyleBackColor = true;
            this.ProfilePLot.Click += new System.EventHandler(this.ProfilePLot_Click);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 24F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(285, 10);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(401, 37);
            this.label2.TabIndex = 8;
            this.label2.Text = "Make More Headroom App";
            // 
            // richTextBox1
            // 
            this.richTextBox1.Location = new System.Drawing.Point(12, 314);
            this.richTextBox1.Name = "richTextBox1";
            this.richTextBox1.Size = new System.Drawing.Size(903, 109);
            this.richTextBox1.TabIndex = 9;
            this.richTextBox1.Text = "Solve\nPlot Circuit power\nhelp";
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(12, 250);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(200, 44);
            this.button1.TabIndex = 10;
            this.button1.Text = "Do Command:";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // ResultBox
            // 
            this.ResultBox.Location = new System.Drawing.Point(12, 429);
            this.ResultBox.Name = "ResultBox";
            this.ResultBox.Size = new System.Drawing.Size(903, 37);
            this.ResultBox.TabIndex = 11;
            this.ResultBox.Text = "";
            // 
            // button2
            // 
            this.button2.Location = new System.Drawing.Point(242, 250);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(149, 44);
            this.button2.TabIndex = 12;
            this.button2.Text = "Show OpenDSS Window";
            this.button2.UseVisualStyleBackColor = true;
            this.button2.Click += new System.EventHandler(this.button2_Click);
            // 
            // SetVregBtn
            // 
            this.SetVregBtn.Location = new System.Drawing.Point(623, 179);
            this.SetVregBtn.Name = "SetVregBtn";
            this.SetVregBtn.Size = new System.Drawing.Size(161, 51);
            this.SetVregBtn.TabIndex = 13;
            this.SetVregBtn.Text = "Set Vreg";
            this.SetVregBtn.UseVisualStyleBackColor = true;
            this.SetVregBtn.Click += new System.EventHandler(this.SetVregBtn_Click);
            // 
            // VregText
            // 
            this.VregText.Location = new System.Drawing.Point(481, 199);
            this.VregText.Name = "VregText";
            this.VregText.Size = new System.Drawing.Size(121, 20);
            this.VregText.TabIndex = 14;
            this.VregText.Text = "124";
            // 
            // button3
            // 
            this.button3.Location = new System.Drawing.Point(796, 183);
            this.button3.Name = "button3";
            this.button3.Size = new System.Drawing.Size(158, 36);
            this.button3.TabIndex = 15;
            this.button3.Text = "Add EVs";
            this.button3.UseVisualStyleBackColor = true;
            this.button3.Click += new System.EventHandler(this.button3_Click);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(966, 478);
            this.Controls.Add(this.button3);
            this.Controls.Add(this.VregText);
            this.Controls.Add(this.SetVregBtn);
            this.Controls.Add(this.button2);
            this.Controls.Add(this.ResultBox);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.richTextBox1);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.ProfilePLot);
            this.Controls.Add(this.Reg4Adjust);
            this.Controls.Add(this.ShowBtn);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.solvebtn);
            this.Controls.Add(this.FileNameBox);
            this.Controls.Add(this.compilebtn);
            this.Controls.Add(this.StartButton);
            this.Name = "Form1";
            this.Text = "Form1";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button StartButton;
        private System.Windows.Forms.Button compilebtn;
        private System.Windows.Forms.TextBox FileNameBox;
        private System.Windows.Forms.Button solvebtn;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button ShowBtn;
        private System.Windows.Forms.Button Reg4Adjust;
        private System.Windows.Forms.Button ProfilePLot;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.RichTextBox richTextBox1;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.RichTextBox ResultBox;
        private System.Windows.Forms.Button button2;
        private System.Windows.Forms.Button SetVregBtn;
        private System.Windows.Forms.TextBox VregText;
        private System.Windows.Forms.Button button3;
    }
}

