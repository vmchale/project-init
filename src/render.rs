//! Module containing functions for rendering templates
extern crate rustache;

use std::fs;
use std::fs::File;
use std::io::Cursor;
use self::rustache::*;
use std::io::prelude::*;
use std::os::unix::fs::PermissionsExt;
use std::process::*;

/// Trait allowing us to create dirs/templates/files.
pub trait Create {
    fn create_dirs(&self, name: &str) -> ();
}

/// Render a list of directories, substituting in templates 
pub fn render_dirs (dirs_pre: Vec<String>, hash: &HashBuilder, name: &str) {
    // substitute into directory names using templates
    let dirs: Vec<String> = dirs_pre.into_iter()
                               .map(|file| { let mut o = Cursor::new(Vec::new());
                                   hash.render(&file, &mut o).unwrap();
                                   String::from_utf8(o.into_inner()).unwrap() })
                               .collect();

    // create directories
    dirs.create_dirs(name);
} 

/// Create all the files, and return a list of files that have been created suitable for insertion
/// into a `HashBuilder`
pub fn render_files<'a>(files_pre: Vec<String>, hash: &HashBuilder, name: &str) -> VecBuilder<'a> {
    
    // render filenames
    let substitutions: Vec<String> = files_pre.into_iter()
                               .map(|file| { let mut o = Cursor::new(Vec::new());
                                   hash.render(&file, &mut o).unwrap();
                                   String::from_utf8(o.into_inner()).unwrap()}).collect();

    // write files 
    let _ = substitutions.clone().into_iter()
        .map(|path| { let mut full_path = name.to_string() ; 
            full_path.push('/') ;
            full_path.push_str(&path) ; 
            File::create(full_path) } ).count();

    // collect filenames
    let s: Vec<Data> = substitutions.into_iter()
        .map(Data::from)
        .collect();

    // return a `VecBuilder` object.
    VecBuilder { data: s }
}
    
/// Create directories given a Vec<String> of directory names
impl <T:ToString>Create for Vec<T> {
    fn create_dirs(&self, name: &str) -> () {
    self.into_iter()
        .map(|dir| { let mut subdir = name.to_string() ;
            subdir.push('/') ;
            subdir.push_str(&dir.to_string()) ;
            fs::create_dir(subdir) } ).count();
    }
}

/// render a `<Vec<String>>` of templates, doing nothing if it's empty.
pub fn render_templates(project: &str, name: &str, hash: &HashBuilder, templates_pre: Option<Vec<String>>, executable: bool) -> () {
    if let Some(t) = templates_pre {

        // create Vec<T> of paths to templates
        let templates: Vec<String> = t.clone().into_iter()
            .map(|file| { let mut p = project.to_string();
                p.push('/');
                p.push_str(&file);
                p } ).collect();

        // read all the template files
        let template_files: Vec<String> = templates.into_iter()
            .map(|p| { 
                let template_f_pre = File::open(&p) ;
                let mut t = String::new();
                let mut template_f = 
                    if let Ok(f) = template_f_pre {
                        f
                    }
                    else {
                        println!("Failed to open file: {:?}", p);
                        exit(0x0f00);
                    };
                template_f
                    .read_to_string(&mut t)
                    .expect("File read failed."); // ok to panic because we already errored. 
                t }).collect();

        // create Vec<T> of paths to rendered templates
        let templates_new: Vec<String> = t.into_iter()
            .map(|file| { let mut p = name.to_string();
                p.push('/');
                p.push_str(&file);
                p }).collect();

        // subtitute into template names
        let templates_named: Vec<String> = templates_new.into_iter()
                                   .map(|n| { let mut o = Cursor::new(Vec::new());
                                       hash.render(&n, &mut o).unwrap();
                                       String::from_utf8(o.into_inner()).unwrap() })
                                   .collect();

        // render all the template files
        let s: Vec<String> = template_files.clone().into_iter()
                                   .map(|file| { let mut o = Cursor::new(Vec::new());
                                       hash.render(&file, &mut o).unwrap();
                                       String::from_utf8(o.into_inner()).unwrap()})
                                   .collect();

        // write the rendered templates
        let files_to_write = templates_named.iter().zip(s.iter());
        let _ = files_to_write.into_iter()
            .map(|(path, contents)| { 
                let c = File::create(&path);
                if let Ok(mut f) = c {
                    let _ = f.write(contents.as_bytes());
                }
                else {
                    println!("Failed to create file: {:?}. Check that the directory is included in your template.toml", path);
                    exit(0x0f00);
                };
                if executable {
                        let mut p = fs::metadata(path)
                            .expect("failed to read file metadata")
                            .permissions();
                        p.set_mode(0o755);
                        let _ = fs::set_permissions(path, p);
                };
            }
            ).count();
    }
}

/// Function to write a file from a static string
pub fn create_file(static_contents: &'static str, name: &str, filename: &str) -> () {
    let mut p = name.to_string();
    p.push('/');
    p.push_str(filename);
    let mut c = File::create(p)
        .expect("File creation failed."); // ok to panic because this is for built-ins.
    let _ = c.write(static_contents.as_bytes());
}

/// Write a file from a static string
pub fn write_file_plain(static_contents: &'static str, name: &str, filename: &str) -> () {
    // write the file
    let mut p = name.to_string();
    p.push('/');
    p.push_str(filename);
    
    // write the rendered template
    let mut c = File::create(p)
        .expect("File creation failed."); // ok to panic because this is for built-ins.
    let _ = c.write(static_contents.as_bytes());

}

/// Render a static string and write it to file
pub fn render_file(static_template: &'static str, name: &str, filename: &str, hash: &HashBuilder) -> () {
    // render the template
    let mut o = Cursor::new(Vec::new());
    hash.render(static_template, &mut o).unwrap();
    let contents = String::from_utf8(o.into_inner()).unwrap();

    // write the file
    let mut p = name.to_string();
    p.push('/');
    p.push_str(filename);
    
    // write the rendered template
    let c = File::create(&p);
    if let Ok(mut f) = c {
        let _ = f.write(contents.as_bytes());
    }
    else {
        println!("Failed to create file: {:?}. Check that the directory is included in your template.toml", p);
        exit(0x0f00);
    }
}
