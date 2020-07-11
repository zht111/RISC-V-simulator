#include<cstdio>
//#include<iostream>
using namespace std;
typedef unsigned int uint;
int x[32],pc;
int lst,lst2;
int vis[32],upd[32],wt[32];
const int inf=1e8;
uint mem[300000];
uint bin[32];
int _end,ti;
int val[1<<16];
int _flag;
int pau;
int cnt;
// branch predictor
const int N=16;
int fail_cnt,total;
int state[1<<N]; // 00 01 10 11
int table[1<<N];
int BP(int _pc)
{
	return (state[_pc&bin[N]]>>1)?table[_pc&bin[N]]:_pc+4;
}
void report(int _pc,int st,int de)
{
	_pc=_pc&bin[N];
	if(st)
	{
		++state[_pc];
		if(state[_pc]>3) state[_pc]=3;
		if(state[_pc]==2) table[_pc]=de;
	}
	else
	{
		--state[_pc];
		if(state[_pc]<0) state[_pc]=0;
	}
}
// end of branch predictor
inline bool invalid(int pid)
{
	return val[pid&bin[16]]!=(pid>>16);
}
struct info{	//Êý¾ÝÔÝ´æÇø 
	int pid;
	int pos,ppos;
	uint op;
	uint opcode; 
	int opr;
	int rd,rs1,rs2;
	int xrs1,xrs2;
	int imm;
	int tmp_pc;
	int wait;
}q[5];
bool ex[5];
int sext(uint x,int len)
{
	uint tmp=x;
	if(x>>(len-1)&1)
	{
		for(int i=len;i<=31;++i)
			tmp|=(1u<<i);
	}
	return (int)tmp;
}
void input()
{
	uint da=0,ptr=0;
	char tmp[20];
	while(~scanf("%s",tmp))
	{
		if(tmp[0]=='@')
		{
			sscanf(tmp+1,"%x",&ptr);
		}
		else
		{
			sscanf(tmp,"%x",&da);
			mem[ptr]=da;
			++ptr;
		}
	}
}
uint scan(int pos,int sz=4)
{
	uint re=0;
	for(int i=sz-1;i>=0;--i)
		re=(re<<8)+mem[pos+i];
	return re;
}
void IF()
{
	if(_end) return;
	if(pau)
	{
		--pau;
		return;
	}
	int pos=pc;
	uint t=scan(pos);
	info tmp;
	tmp.op=t;
	tmp.pid=ti;
	tmp.pos=pc;	
//cerr<<"IF pid="<<tmp.pid<<endl;
	val[tmp.pid&bin[16]]=(tmp.pid>>16);
	//pc+=4;
	pc=BP(pc);
	tmp.ppos=pc;
	q[1]=tmp;ex[1]=1;
}
void ID()
{
	if(!ex[1]) return;
	info &c=q[1];
	if(invalid(c.pid))
	{
		ex[1]=0;
		return;
	}
//cerr<<"ID pid="<<dec<<c.pid<<endl;

	int pid=c.pid;
	uint op=c.op;
	uint opcode=op&bin[7];
	int rs1=-1,rs2=-1,rd=-1,imm=0;
	int opr=0;
	_flag=0;
	
//cerr<<"pid="<<dec<<pid<<" ";
	
	if(opcode==55)
	{
		rd=(op>>7)&bin[5];
		imm=(op>>12)<<12;
//		cerr<<"lui rd="<<dec<<rd<<",imm="<<hex<<imm<<endl;
		opr=1;
	}
	else if(opcode==23)
	{
		rd=(op>>7)&bin[5];
		imm=(op>>12)<<12;
//		cerr<<"auipc rd="<<dec<<rd<<",imm="<<hex<<imm<<endl;
		opr=2;
	}
	else if(opcode==111)
	{
		rd=(op>>7)&bin[5];
		uint tmp=0;
		if(op>>31&1) tmp|=(1u<<20);
		if(op>>20&1) tmp|=(1u<<11);
		for(int i=0;i<10;++i)
		if(op>>(i+21)&1) tmp|=(1u<<i+1);
		for(int i=11;i<19;++i)
		if(op>>(i+1)&1) tmp|=(1u<<i+1);
		imm=sext(tmp,21);
//		cerr<<"jal rd="<<dec<<rd<<",imm="<<hex<<imm<<endl;
		opr=3;++total;
	}
	else if(opcode==103)
	{
		rd=(op>>7)&bin[5];
		rs1=(op>>15)&bin[5];
		imm=sext(op>>20,12);
//		cerr<<"jalr rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
		opr=4;++total;
	}
	else if(opcode==99)
	{
		uint opcode2=(op>>12)&bin[3];
		rs1=(op>>15)&bin[5];
		rs2=(op>>20)&bin[5];
		uint tmp=0;
		if(op>>31&1) tmp|=(1u<<12);
		if(op>>7&1) tmp|=(1u<<11);
		for(int i=8;i<12;++i)
		if(op>>i&1) tmp|=(1u<<i-7);
		for(int i=25;i<31;++i)
		if(op>>i&1) tmp|=(1u<<i-20);
		imm=sext(tmp,13);
		if(opcode2==0)
		{
//			cerr<<"beq rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr=5;++total;
		}
		else if(opcode2==1)
		{
//			cerr<<"bne rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr=6;++total;
		}
		else if(opcode2==4)
		{
//			cerr<<"blt rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr=7;++total;
		}
		else if(opcode2==5)
		{
//			cerr<<"bge rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr=8;++total;
		}
		else if(opcode2==6)
		{
//			cerr<<"bltu rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr=9;++total;
		}
		else if(opcode2==7)
		{
//			cerr<<"bgeu rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr=10;++total;
		}
	}
	else if(opcode==3)
	{
		uint opcode2=(op>>12)&bin[3];
		rd=(op>>7)&bin[5];
		rs1=(op>>15)&bin[5];
		imm=sext(op>>20,12);
		if(opcode2==0)
		{
//			cerr<<"lb rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr=11;_flag=2;
		}
		else if(opcode2==1)
		{
//			cerr<<"lh rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr=12;_flag=2;
		}
		else if(opcode2==2)
		{
//			cerr<<"lw rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr=13;_flag=2;
		}
		else if(opcode2==4)
		{
//			cerr<<"lbu rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr=14;_flag=2;
		}
		else if(opcode2==5)
		{
//			cerr<<"lhu rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr=15;_flag=2;
		}
	}
	else if(opcode==35)
	{
		uint opcode2=(op>>12)&bin[3];
		rs1=(op>>15)&bin[5];
		rs2=(op>>20)&bin[5];
		uint tmp=0;
		for(int i=7;i<12;++i)
		if(op>>i&1) tmp|=(1u<<i-7);
		for(int i=25;i<32;++i)
		if(op>>i&1) tmp|=(1u<<i-20);
		imm=sext(tmp,12);
		if(opcode2==0)
		{
//			cerr<<"sb rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr=16;_flag=2;
		}
		else if(opcode2==1)
		{
//			cerr<<"sh rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr=17;_flag=2;
		}
		else if(opcode2==2)
		{
//			cerr<<"sw rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr=18;_flag=2;
		}
	}
	else if(opcode==19)
	{
		uint opcode2=(op>>12)&bin[3];
		rd=(op>>7)&bin[5];
		rs1=(op>>15)&bin[5];
		imm=sext(op>>20,12);
		if(opcode2==0)
		{
//			cerr<<"addi rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr=19;
		}
		else if(opcode2==2)
		{
//			cerr<<"slti rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr=20;
		}
		else if(opcode2==3)
		{
//			cerr<<"sltiu rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr=21;
		}
		else if(opcode2==4)
		{
//			cerr<<"xori rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr=22;
		}
		else if(opcode2==6)
		{
//			cerr<<"ori rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr=23;
		}
		else if(opcode2==7)
		{
//			cerr<<"andi rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr=24;
		}
		
		else if(opcode2==1)
		{
			imm=(op>>20)&bin[5];
//			cerr<<"slli rs1="<<dec<<rs1<<",rd="<<rd<<",shamt="<<hex<<imm<<endl;
			opr=25;
		}
		else if(opcode2==5)
		{
			imm=(op>>20)&bin[5];
			if(op>>30&1)
			{
//				cerr<<"srai rs1="<<dec<<rs1<<",rd="<<rd<<",shamt="<<hex<<imm<<endl;
				opr=27;
			}
			else
			{
//				cerr<<"srli rs1="<<dec<<rs1<<",rd="<<rd<<",shamt="<<hex<<imm<<endl;
				opr=26;
			}
		}
	}
	else if(opcode==51)
	{
		uint opcode2=(op>>12)&bin[3];
		rd=(op>>7)&bin[5];
		rs1=(op>>15)&bin[5];
		rs2=(op>>20)&bin[5];
		uint opcode3=op>>30&1;
		if(opcode2==0)
		{
			if(opcode3==0)
			{
//				cerr<<"add rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
				opr=28;
			}
			else
			{
//				cerr<<"sub rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
				opr=29;
			}
		}
		else if(opcode2==1)
		{
//			cerr<<"sll rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr=30;
		}
		else if(opcode2==2)
		{
//			cerr<<"slt rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr=31;
		}
		else if(opcode2==3)
		{
//			cerr<<"sltu rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr=32;
		}
		else if(opcode2==4)
		{
//			cerr<<"xor rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr=33;
		}
		else if(opcode2==5)
		{
			if(opcode3==0)
			{
//				cerr<<"srl rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
				opr=34;
			}
			else
			{
//				cerr<<"sra rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
				opr=35;
			}
		}
		else if(opcode2==6)
		{
//			cerr<<"or rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr=36;
		}
		else if(opcode2==7)
		{
//			cerr<<"and rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr=37;
		}
	}
	else if(opcode)
	{
//		cerr<<"invalid opcode: "<<dec<<opcode<<endl;
//		cerr<<"op: "<<hex<<op<<endl;
		return;
	}
	info t;
	t.pid=pid;
	t.opr=opr;
	t.op=op;
	t.rd=rd;
	t.rs1=rs1;
	t.rs2=rs2;
	t.imm=imm;
	t.pos=c.pos;
	t.ppos=c.ppos;
	
	if(rs1>0 && wt[rs1]) {++pau;return;}
	if(rs2>0 && wt[rs2]) {++pau;return;}
	int xrs1=-1,xrs2=-1; 
	if(rs1!=-1)xrs1=x[rs1];
	if(rs2!=-1)xrs2=x[rs2];	
	if(rs1>0 && (vis[rs1]==lst || vis[rs1]==lst2)) xrs1=upd[rs1];
	if(rs2>0 && (vis[rs2]==lst || vis[rs2]==lst2)) xrs2=upd[rs2];
	t.xrs1=xrs1;
	t.xrs2=xrs2;
	
	ex[1]=0;
	q[2]=t;ex[2]=1;
	if(_flag) pau+=_flag,_flag=0;
}
void EX()
{
	if(!ex[2]) return;
	info &c=q[2];
	if(invalid(c.pid))
	{
		ex[2]=0;
		return;
	}
	int pid=c.pid;
	int rs1=c.rs1,rs2=c.rs2;
	int xrs1=c.xrs1,xrs2=c.xrs2,rd=c.rd,imm=c.imm;
	int opr=c.opr;
	int pos=c.pos;
	
	ex[2]=0;
//cerr<<"EX pid="<<dec<<c.pid<<endl;
	
	info t;
	t.pid=pid;
	t.opr=opr;
	t.pos=pos;
	t.rd=rd;
	t.tmp_pc=-1;
	t.opcode=inf;
	t.ppos=c.ppos;

	if(opr==1)
	{
		//x[rd]=imm;
		t.imm=imm;
	}
	else if(opr==2)
	{
		//x[rd]=pos+imm;
		t.imm=pos+imm;
	}
	else if(opr==3)
	{
		//x[rd]=pos+4;
		//pc=pos+imm;
		t.imm=pos+4;
		t.tmp_pc=pos+imm;
	}
	else if(opr==4)
	{
		int tmp=pos+4;
		//pc=(xrs1+imm)&~1;
		//x[rd]=tmp;
		t.tmp_pc=(xrs1+imm)&~1;
		t.imm=tmp;
	}
	else if(opr==5)
	{
		if(xrs1==xrs2)
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr==6)
	{
		if(xrs1!=xrs2)
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr==7)
	{
		if(xrs1<xrs2)
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr==8)
	{
		if(xrs1>=xrs2)
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr==9)
	{
		if(((uint)xrs1)<((uint)xrs2))
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr==10)
	{
		if(((uint)xrs1)>=((uint)xrs2))
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr==11)
	{
		//x[rd]=sext(scan(xrs1+imm,1),8);
		t.opcode=xrs1+imm;
	}
	else if(opr==12)
	{
		//x[rd]=sext(scan(xrs1+imm,2),16);
		t.opcode=xrs1+imm;
	}
	else if(opr==13)
	{
		//x[rd]=scan(xrs1+imm);
		t.opcode=xrs1+imm;
	}
	else if(opr==14)
	{
		//x[rd]=scan(xrs1+imm,1);
		t.opcode=xrs1+imm;
	}
	else if(opr==15)
	{
		//x[rd]=scan(xrs1+imm,2);
		t.opcode=xrs1+imm;
	}
	else if(opr==16)
	{
		//mem[xrs1+imm]=xrs2&bin[8];
		t.opcode=xrs1+imm;
		t.imm=xrs2&bin[8];
		t.rd=0;
	}
	else if(opr==17)
	{
		//mem[xrs1+imm]=xrs2&bin[16];
		t.opcode=xrs1+imm;
		t.imm=xrs2&bin[16];
		t.rd=0;
	}
	else if(opr==18)
	{
		//mem[xrs1+imm]=xrs2;
		t.opcode=xrs1+imm;
		t.imm=xrs2;
		t.rd=0;
	}
	else if(opr==19)
	{
		//x[rd]=xrs1+imm;
		t.imm=xrs1+imm;
	}
	else if(opr==20)
	{
		if(xrs1<imm) t.imm=1;
		else t.imm=0;
	}
	else if(opr==21)
	{
		if(((uint)xrs1)<((uint)imm)) t.imm=1;
		else t.imm=0;
	}
	else if(opr==22)
	{
		t.imm=xrs1^imm;
	}
	else if(opr==23)
	{
		t.imm=xrs1|imm;
	}
	else if(opr==24)
	{
		t.imm=xrs1&imm;
	}
	else if(opr==25)
	{
		t.imm=xrs1<<imm;
	}
	else if(opr==26)
	{
		t.imm=((uint)xrs1)>>imm;
	}
	else if(opr==27)
	{
		t.imm=xrs1>>imm;
	}
	else if(opr==28)
	{
		t.imm=xrs1+xrs2;
	}
	else if(opr==29)
	{
		t.imm=xrs1-xrs2;
	}
	else if(opr==30)
	{
		t.imm=xrs1<<(((uint)xrs2)&bin[5]);
	}
	else if(opr==31)
	{
		if(xrs1<xrs2) t.imm=1;
		else t.imm=0;
	}
	else if(opr==32)
	{
		if(((uint)xrs1)<((uint)xrs2)) t.imm=1;
		else t.imm=0;
	}
	else if(opr==33)
	{
		t.imm=xrs1^xrs2;
	}
	else if(opr==34)
	{
		t.imm=((uint)xrs1)>>(((uint)xrs2)&bin[5]);
	}
	else if(opr==35)
	{
		t.imm=xrs1>>(((uint)xrs2)&bin[5]);
	}
	else if(opr==36)
	{
		t.imm=xrs1|xrs2;
	}
	else if(opr==37)
	{
		t.imm=xrs1&xrs2;
	}
	if(t.opcode!=inf)
	{
		t.wait=2;
		if(t.rd) lst2=lst,lst=vis[t.rd]=t.pid,wt[t.rd]^=t.pid;
		else lst2=lst,lst=t.pid;
	}
	else
	{
		if(t.rd) lst2=lst,lst=vis[t.rd]=t.pid,upd[t.rd]=t.imm;
		else lst2=lst,lst=t.pid;
	}
	t.op=c.op;
	q[3]=t;ex[3]=1;
}
void MEM()
{
	if(!ex[3]) return;
	info &c=q[3];
	if(invalid(c.pid))
	{
		ex[3]=0;
		return;
	}		
//cerr<<"MEM pid="<<c.pid<<endl;

	if(c.opcode==inf)
	{
		ex[3]=0;
		q[4]=c;ex[4]=1;
		int tmp_pc=c.tmp_pc;
		if(tmp_pc==-1)
		{
			tmp_pc=c.pos+4;
			report(c.pos,0,0);
		}
		else report(c.pos,1,tmp_pc);
		if(tmp_pc!=c.ppos)
		{
			for(int i=c.pid+1;i<ti;++i) val[i&bin[16]]=-1;
			pc=tmp_pc;
			++fail_cnt;
		}
		return;
	}
	if(c.wait==0)
	{
		ex[3]=0;
		q[4]=c;ex[4]=1;
	}
	else c.wait--;
	if(c.wait<1) return;
		
	int opr=c.opr;
	info &t=c;
	if(opr==11)
	{
		t.imm=sext(scan(c.opcode,1),8);
	}
	else if(opr==12)
	{
		t.imm=sext(scan(c.opcode,2),16);
	}
	else if(opr==13)
	{
		t.imm=scan(c.opcode);
	}
	else if(opr==14)
	{
		t.imm=scan(c.opcode,1);
	}
	else if(opr==15)
	{
		t.imm=scan(c.opcode,2);
	}
	else if(opr==18||opr==17||opr==16)
	{
		mem[c.opcode]=c.imm;
	}
	if(t.rd) upd[t.rd]=t.imm,wt[t.rd]^=t.pid;	
}
void WB()
{
	if(!ex[4]) return;
	info &c=q[4];
	ex[4]=0;
	if(invalid(c.pid)) return;	
//cerr<<"WB pid="<<c.pid<<endl;
	int rd=c.rd;
	int imm=c.imm;
	if(c.op==267388179u) _end=ti+5;
	else if(rd) x[rd]=imm;
	++cnt;
}
int main()
{
//	freopen("pi.data","r",stdin);
//	freopen("output.txt","w",stderr);
	bin[0]=1;
	for(int i=1;i<32;++i) bin[i]=bin[i-1]<<1;
	for(int i=0;i<32;++i) --bin[i];
	input();
	pc=0;
	for(ti=1<<16;(!_end)||ti<=_end;++ti)
	{
		//IF(); ID(); EX(); MEM(); WB();
		WB(); MEM(); EX(); ID(); IF();
	}
	printf("%u",((uint)x[10])&255u);
	
/*
cerr<<"\nbranch prediction: "<<total-fail_cnt<<"/"<<total<<" ";
if(total) cerr<<100.0*(total-fail_cnt)/total<<"%"<<endl;
*/

//	fclose(stderr);
//	system("start output.txt");	
	return 0;
}
