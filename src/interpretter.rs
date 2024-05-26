use std::io;
use std::io::Write;
use std::sync::mpsc;
use std::sync::mpsc::RecvTimeoutError;
use std::thread;
use std::time::Duration;

use crate::ast::AstPrgPart;
use crate::ast::AstTagDecl;
use crate::callstack::CallStack;
use crate::callstack::MemTableVal;
use crate::parser::SprocketParser;
use crate::semantics::SemanticAnalyzer;
use crate::sprocket::SprocketError;
use crate::sprocket::SprocketResult;
use crate::symbol::SymbolKind;

pub struct SprocketInterpretter {
    pub is_running: bool,
    pub call_stack: CallStack,
}

impl SprocketInterpretter {
    pub fn new(source: &str) -> SprocketResult<Self> {
        let mut parser = SprocketParser::new();
        let ast = parser.parse(source)?;

        let mut sem_analyzer = SemanticAnalyzer::new();
        let mut callstack = CallStack::new();
        callstack.push(None);
        callstack.load_globals()?;
        callstack.insert_symbol("__main__", SymbolKind::Task(ast.clone()))?;
        sem_analyzer.analyze(&ast, &mut callstack)?;
        callstack.load_default_vals();

        let main_task = match callstack.lookup_task("__main__")? {
            Some(task) => task.clone(),
            None => return Err(SprocketError::RuntimeTypeError),
        };
        for part in main_task {
            if let AstPrgPart::TagDecl(AstTagDecl { .. }) = part {
                callstack.exe_prg_part(&part)?;
            }
        }
        println!("{:?}", &callstack);
        Ok(Self {
            is_running: false,
            call_stack: callstack,
        })
    }

    pub fn run(mut self) -> SprocketResult<()> {
        let (cli_tx, cli_rx) = mpsc::channel::<InterpCliMsg>();
        let (_prg_tx, _prg_rx) = mpsc::channel::<InterpPrgMsg>();
        let cli_thread_handle = thread::spawn(move || {
            let mut stdout = io::stdout();
            let mut buf = String::new();
            let mut is_interp_running = false;
            loop {
                buf.clear();
                stdout.write_all(b"\xE2\x9A\x99 ").unwrap();
                write!(
                    stdout,
                    "|{}|> ",
                    if is_interp_running {
                        "RUNNING"
                    } else {
                        "STOPPED"
                    }
                )
                .unwrap();
                stdout.flush().unwrap();
                match io::stdin().read_line(&mut buf) {
                    Ok(_) => {}
                    Err(_) => {}
                };
                buf = buf.trim().to_string();
                // TODO: should probably setup a proper CLI for this
                let args: Vec<_> = buf.split(' ').collect();
                if args.len() <= 0 {
                    continue;
                }
                if args[0] == "exit" {
                    cli_tx.send(InterpCliMsg::Exit).unwrap();
                    break;
                } else if args[0] == "start" {
                    cli_tx.send(InterpCliMsg::Start).unwrap();
                    is_interp_running = true;
                } else if args[0] == "stop" {
                    cli_tx.send(InterpCliMsg::Stop).unwrap();
                    is_interp_running = false;
                } else if args[0] == "step" {
                    cli_tx.send(InterpCliMsg::Step).unwrap();
                } else if args[0] == "get" {
                    if args.len() == 1 {
                        continue;
                    }
                    cli_tx.send(InterpCliMsg::Get(args[1].to_string())).unwrap();
                } else if args[0] == "set" {
                    if args.len() == 1 {
                        continue;
                    }
                    cli_tx.send(InterpCliMsg::Set(args[1].to_string())).unwrap();
                } else if args[0] == "clear" {
                    if args.len() == 1 {
                        continue;
                    }
                    cli_tx
                        .send(InterpCliMsg::Clear(args[1].to_string()))
                        .unwrap();
                } else {
                    println!("unknown cmd");
                }
            }
        });
        loop {
            if self.is_running {
                self.update()?;
            }
            match cli_rx.recv_timeout(Duration::from_millis(10)) {
                Ok(InterpCliMsg::Exit) => {
                    break;
                }
                Ok(InterpCliMsg::Start) => {
                    if self.is_running {
                        println!("Program is already running");
                    } else {
                        self.is_running = true;
                    }
                }
                Ok(InterpCliMsg::Stop) => {
                    if self.is_running {
                        self.is_running = false;
                    } else {
                        println!("Program is already stopped");
                    }
                }
                Ok(InterpCliMsg::Step) => {
                    if !self.is_running {
                        self.update()?;
                    }
                }
                Ok(InterpCliMsg::Get(id)) => {
                    let msg = match &self.call_stack.lookup_symbol_kind(&id) {
                        Some(SymbolKind::Var(_)) => {
                            match self.call_stack.lookup_symbol_val(&id)? {
                                Some(MemTableVal::Bool(val)) => val.to_string(),
                                Some(MemTableVal::Int32(val)) => val.to_string(),
                                Some(MemTableVal::_RefTo(id)) => format!("RefTo({})", id),
                                None => return Err(SprocketError::SymbolMissingVal(id.clone())),
                            }
                        }
                        Some(SymbolKind::FunctionDef(_)) => "{function}".to_string(),
                        Some(SymbolKind::Type(_)) => "{type}".to_string(),
                        Some(SymbolKind::Task(_)) => "{task}".to_string(),
                        None => "error: unknown symbol".to_string(),
                    };
                    println!("{}", msg);
                }
                Err(RecvTimeoutError::Timeout) => {}
                Err(RecvTimeoutError::Disconnected) => {
                    break;
                }
                Ok(InterpCliMsg::Set(id)) => {
                    self.call_stack
                        .set_symbol_val(&id, MemTableVal::Bool(true))?;
                }
                Ok(InterpCliMsg::Clear(id)) => {
                    self.call_stack
                        .set_symbol_val(&id, MemTableVal::Bool(false))?;
                }
            }
        }
        cli_thread_handle.join().unwrap();
        Ok(())
    }

    fn update(&mut self) -> SprocketResult<()> {
        let main_task = match self.call_stack.lookup_task("__main__")? {
            Some(task) => task.clone(),
            None => return Err(SprocketError::RuntimeTypeError),
        };
        for part in main_task {
            match part {
                AstPrgPart::Statement(_) => self.call_stack.exe_prg_part(&part)?,
                _ => {}
            }
        }
        Ok(())
    }
}

pub enum InterpCliMsg {
    Exit,
    Start,
    Stop,
    Step,
    Get(String),
    Set(String),
    Clear(String),
}

pub enum InterpPrgMsg {
    _ModeChangeResponse(Result<(), String>),
    _GetExprResponse(Result<String, String>),
}
