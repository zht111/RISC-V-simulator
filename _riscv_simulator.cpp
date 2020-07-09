#include<cstdio>
#include<cstring>
#include<iostream>
#include<string>
#include<queue>
using namespace std;
typedef unsigned int uint;
int x[32],pc;
int lst,lst2;
int vis[32],upd[32],wt[32];
const int inf=1e8;
uint mem[1000000];
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
bool invalid(int pid)
{
	return val[pid&bin[16]]!=(pid>>16);
}
struct info{	//Êý¾ÝÔÝ´æÇø 
	int pid;
	int pos,ppos;
	uint op;
	uint opcode; 
	string opr;
	int rd,rs1,rs2;
	int xrs1,xrs2;
	int imm;
	int tmp_pc;
	int wait;
};
std::queue<info>q[5];
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
	q[1].push(tmp);
}
void ID()
{
	if(q[1].empty()) return;
	info c=q[1].front();
	if(invalid(c.pid))
	{
		q[1].pop();
		return;
	}
//cerr<<"ID pid="<<dec<<c.pid<<endl;

	int pid=c.pid;
	uint op=c.op;
	uint opcode=op&bin[7];
	int rs1=-1,rs2=-1,rd=-1,imm=0;
	string opr="";
	_flag=0;
	
//cerr<<"pid="<<dec<<pid<<" ";
	
	if(opcode==55)
	{
		rd=(op>>7)&bin[5];
		imm=(op>>12)<<12;
//		cerr<<"lui rd="<<dec<<rd<<",imm="<<hex<<imm<<endl;
		opr="lui";
	}
	else if(opcode==23)
	{
		rd=(op>>7)&bin[5];
		imm=(op>>12)<<12;
//		cerr<<"auipc rd="<<dec<<rd<<",imm="<<hex<<imm<<endl;
		opr="auipc";
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
		opr="jal";++total;
	}
	else if(opcode==103)
	{
		rd=(op>>7)&bin[5];
		rs1=(op>>15)&bin[5];
		imm=sext(op>>20,12);
//		cerr<<"jalr rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
		opr="jalr";++total;
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
			opr="beq";++total;
		}
		else if(opcode2==1)
		{
//			cerr<<"bne rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr="bne";++total;
		}
		else if(opcode2==4)
		{
//			cerr<<"blt rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr="blt";++total;
		}
		else if(opcode2==5)
		{
//			cerr<<"bge rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr="bge";++total;
		}
		else if(opcode2==6)
		{
//			cerr<<"bltu rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr="bltu";++total;
		}
		else if(opcode2==7)
		{
//			cerr<<"bgeu rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr="bgeu";++total;
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
			opr="lb";_flag=2;
		}
		else if(opcode2==1)
		{
//			cerr<<"lh rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr="lh";_flag=2;
		}
		else if(opcode2==2)
		{
//			cerr<<"lw rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr="lw";_flag=2;
		}
		else if(opcode2==4)
		{
//			cerr<<"lbu rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr="lbu";_flag=2;
		}
		else if(opcode2==5)
		{
//			cerr<<"lhu rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr="lhu";_flag=2;
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
			opr="sb";_flag=2;
		}
		else if(opcode2==1)
		{
//			cerr<<"sh rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr="sh";_flag=2;
		}
		else if(opcode2==2)
		{
//			cerr<<"sw rs1="<<dec<<rs1<<",rs2="<<rs2<<",imm="<<hex<<imm<<endl;
			opr="sw";_flag=2;
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
			opr="addi";
		}
		else if(opcode2==2)
		{
//			cerr<<"slti rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr="slti";
		}
		else if(opcode2==3)
		{
//			cerr<<"sltiu rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr="sltiu";
		}
		else if(opcode2==4)
		{
//			cerr<<"xori rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr="xori";
		}
		else if(opcode2==6)
		{
//			cerr<<"ori rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr="ori";
		}
		else if(opcode2==7)
		{
//			cerr<<"andi rs1="<<dec<<rs1<<",rd="<<rd<<",imm="<<hex<<imm<<endl;
			opr="andi";
		}
		
		else if(opcode2==1)
		{
			imm=(op>>20)&bin[5];
//			cerr<<"slli rs1="<<dec<<rs1<<",rd="<<rd<<",shamt="<<hex<<imm<<endl;
			opr="slli";
		}
		else if(opcode2==5)
		{
			imm=(op>>20)&bin[5];
			if(op>>30&1)
			{
//				cerr<<"srai rs1="<<dec<<rs1<<",rd="<<rd<<",shamt="<<hex<<imm<<endl;
				opr="srai";
			}
			else
			{
//				cerr<<"srli rs1="<<dec<<rs1<<",rd="<<rd<<",shamt="<<hex<<imm<<endl;
				opr="srli";
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
				opr="add";
			}
			else
			{
//				cerr<<"sub rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
				opr="sub";
			}
		}
		else if(opcode2==1)
		{
//			cerr<<"sll rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr="sll";
		}
		else if(opcode2==2)
		{
//			cerr<<"slt rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr="slt";
		}
		else if(opcode2==3)
		{
//			cerr<<"sltu rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr="sltu";
		}
		else if(opcode2==4)
		{
//			cerr<<"xor rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr="xor";
		}
		else if(opcode2==5)
		{
			if(opcode3==0)
			{
//				cerr<<"srl rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
				opr="srl";
			}
			else
			{
//				cerr<<"sra rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
				opr="sra";
			}
		}
		else if(opcode2==6)
		{
//			cerr<<"or rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr="or";
		}
		else if(opcode2==7)
		{
//			cerr<<"and rs1="<<dec<<rs1<<",rs2="<<rs2<<",rd="<<rd<<endl;
			opr="and";
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
	
	q[1].pop();	
	q[2].push(t);
	if(_flag) pau+=_flag,_flag=0;
}
void EX()
{
	if(q[2].empty()) return;
	info c=q[2].front();
	if(invalid(c.pid))
	{
		q[2].pop();
		return;
	}
	int pid=c.pid;
	int rs1=c.rs1,rs2=c.rs2;
	int xrs1=c.xrs1,xrs2=c.xrs2,rd=c.rd,imm=c.imm;
	string opr=c.opr;
	int pos=c.pos;
	
	q[2].pop();	
//cerr<<"EX pid="<<dec<<c.pid<<endl;

int xxrs1=0,xxrs2=0;
if(rs1!=-1)xxrs1=x[rs1];
if(rs2!=-1)xxrs2=x[rs2];	

if(rs1>0 && (vis[rs1]==lst || vis[rs1]==lst2)) xxrs1=upd[rs1];
if(rs2>0 && (vis[rs2]==lst || vis[rs2]==lst2)) xxrs2=upd[rs2];

if(rs1>0 && xxrs1!=xrs1)exit(101);
if(rs2>0 && xxrs2!=xrs2)exit(102);
	
	info t;
	t.pid=pid;
	t.opr=opr;
	t.pos=pos;
	t.rd=rd;
	t.tmp_pc=-1;
	t.opcode=inf;
	t.ppos=c.ppos;

	if(opr=="lui")
	{
		//x[rd]=imm;
		t.imm=imm;
	}
	else if(opr=="aupic")
	{
		//x[rd]=pos+imm;
		t.imm=pos+imm;
	}
	else if(opr=="jal")
	{
		//x[rd]=pos+4;
		//pc=pos+imm;
		t.imm=pos+4;
		t.tmp_pc=pos+imm;
	}
	else if(opr=="jalr")
	{
		int tmp=pos+4;
		//pc=(xrs1+imm)&~1;
		//x[rd]=tmp;
		t.tmp_pc=(xrs1+imm)&~1;
		t.imm=tmp;
	}
	else if(opr=="beq")
	{
		if(xrs1==xrs2)
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr=="bne")
	{
		if(xrs1!=xrs2)
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr=="blt")
	{
		if(xrs1<xrs2)
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr=="bge")
	{
		if(xrs1>=xrs2)
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr=="bltu")
	{
		if(((uint)xrs1)<((uint)xrs2))
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr=="bgeu")
	{
		if(((uint)xrs1)>=((uint)xrs2))
		{
			//pc=pos+imm;
			t.tmp_pc=pos+imm;
		}
		t.rd=0;
	}
	else if(opr=="lb")
	{
		//x[rd]=sext(scan(xrs1+imm,1),8);
		t.opcode=xrs1+imm;
	}
	else if(opr=="lh")
	{
		//x[rd]=sext(scan(xrs1+imm,2),16);
		t.opcode=xrs1+imm;
	}
	else if(opr=="lw")
	{
		//x[rd]=scan(xrs1+imm);
		t.opcode=xrs1+imm;
	}
	else if(opr=="lbu")
	{
		//x[rd]=scan(xrs1+imm,1);
		t.opcode=xrs1+imm;
	}
	else if(opr=="lhu")
	{
		//x[rd]=scan(xrs1+imm,2);
		t.opcode=xrs1+imm;
	}
	else if(opr=="sb")
	{
		//mem[xrs1+imm]=xrs2&bin[8];
		t.opcode=xrs1+imm;
		t.imm=xrs2&bin[8];
		t.rd=0;
	}
	else if(opr=="sh")
	{
		//mem[xrs1+imm]=xrs2&bin[16];
		t.opcode=xrs1+imm;
		t.imm=xrs2&bin[16];
		t.rd=0;
	}
	else if(opr=="sw")
	{
		//mem[xrs1+imm]=xrs2;
		t.opcode=xrs1+imm;
		t.imm=xrs2;
		t.rd=0;
	}
	else if(opr=="addi")
	{
		//x[rd]=xrs1+imm;
		t.imm=xrs1+imm;
	}
	else if(opr=="slti")
	{
		if(xrs1<imm) t.imm=1;
		else t.imm=0;
	}
	else if(opr=="sltiu")
	{
		if(((uint)xrs1)<((uint)imm)) t.imm=1;
		else t.imm=0;
	}
	else if(opr=="xori")
	{
		t.imm=xrs1^imm;
	}
	else if(opr=="ori")
	{
		t.imm=xrs1|imm;
	}
	else if(opr=="andi")
	{
		t.imm=xrs1&imm;
	}
	else if(opr=="slli")
	{
		t.imm=xrs1<<imm;
	}
	else if(opr=="srli")
	{
		t.imm=((uint)xrs1)>>imm;
	}
	else if(opr=="srai")
	{
		t.imm=xrs1>>imm;
	}
	else if(opr=="add")
	{
		t.imm=xrs1+xrs2;
	}
	else if(opr=="sub")
	{
		t.imm=xrs1-xrs2;
	}
	else if(opr=="sll")
	{
		t.imm=xrs1<<(((uint)xrs2)&bin[5]);
	}
	else if(opr=="slt")
	{
		if(xrs1<xrs2) t.imm=1;
		else t.imm=0;
	}
	else if(opr=="sltu")
	{
		if(((uint)xrs1)<((uint)xrs2)) t.imm=1;
		else t.imm=0;
	}
	else if(opr=="xor")
	{
		t.imm=xrs1^xrs2;
	}
	else if(opr=="srl")
	{
		t.imm=((uint)xrs1)>>(((uint)xrs2)&bin[5]);
	}
	else if(opr=="sra")
	{
		t.imm=xrs1>>(((uint)xrs2)&bin[5]);
	}
	else if(opr=="or")
	{
		t.imm=xrs1|xrs2;
	}
	else if(opr=="and")
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
	q[3].push(t);
}
void MEM()
{
	if(q[3].empty()) return;
	info c=q[3].front();
	if(invalid(c.pid))
	{
		q[3].pop();
		return;
	}		
//cerr<<"MEM pid="<<c.pid<<endl;

	if(c.opcode==inf)
	{
		q[3].pop();
		q[4].push(c);
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
		q[3].pop();
		q[4].push(c);
	}
	else q[3].front().wait--;
	if(c.wait<2) return;
	
	int tmp_pc=c.tmp_pc;
	if(tmp_pc!=-1)
	{
		for(int i=c.pid+1;i<ti;++i)
		{
			if(val[i&bin[16]]==i>>16) ++cnt;
			val[i&bin[16]]=-1;
		}
		pc=tmp_pc;
	}	
	
	string opr=c.opr;
	info &t=q[3].front();
	if(opr=="lb")
	{
		t.imm=sext(scan(c.opcode,1),8);
	}
	else if(opr=="lh")
	{
		t.imm=sext(scan(c.opcode,2),16);
	}
	else if(opr=="lw")
	{
		t.imm=scan(c.opcode);
	}
	else if(opr=="lbu")
	{
		t.imm=scan(c.opcode,1);
	}
	else if(opr=="lhu")
	{
		t.imm=scan(c.opcode,2);
	}
	else if(opr=="sb"||opr=="sh"||opr=="sw")
	{
		mem[c.opcode]=c.imm;
	}
	if(t.rd) upd[t.rd]=t.imm,wt[t.rd]^=t.pid;	
}
void WB()
{
	if(q[4].empty()) return;
	info c=q[4].front();
	q[4].pop();
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
	memset(val,-1,sizeof val);
//	freopen("expr.data","r",stdin);
//	freopen("output.txt","w",stderr);
	bin[0]=1;
	for(int i=1;i<32;++i) bin[i]=bin[i-1]<<1;
	for(int i=0;i<32;++i) --bin[i];
	input();
	pc=0;
	for(ti=1;(!_end)||ti<=_end;++ti)
	{
		//IF(); ID(); EX(); MEM(); WB();
		WB(); MEM(); EX(); ID(); IF();
//if(!(ti&bin[16]))cerr<<ti<<endl;
	}
	printf("%u",((uint)x[10])&255u);
	
/*	
cerr<<"\nclock="<<dec<<ti<<endl;
cerr<<"count="<<dec<<cnt<<endl;
if(total)
{
cerr<<"branch prediction: "<<total-fail_cnt<<"/"<<total<<" ";
cerr<<100.0*(total-fail_cnt)/total<<"%"<<endl;	
}
*/

//	fclose(stderr);
//	system("start output.txt");	
	return 0;
}
