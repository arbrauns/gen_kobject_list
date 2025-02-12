use std::io::Write;

use color_eyre::Result;

use crate::{kobject_to_enum, StructTags, KOBJECTS};

mod gperf;

pub use gperf::write_gperf_table;

pub fn write_validation_output(mut outfile: impl Write, struct_tags: &StructTags) -> Result<()> {
    writeln!(outfile, "#ifndef DRIVER_VALIDATION_GEN_H")?;
    writeln!(outfile, "#define DRIVER_VALIDATION_GEN_H")?;
    writeln!(
        outfile,
        "#define K_SYSCALL_DRIVER_GEN(ptr, op, driver_lower_case, driver_upper_case) \\"
    )?;
    writeln!(
        outfile,
        "\t\t(K_SYSCALL_OBJ(ptr, K_OBJ_DRIVER_##driver_upper_case) || \\"
    )?;
    writeln!(
        outfile,
        "\t\t K_SYSCALL_DRIVER_OP(ptr, driver_lower_case##_driver_api, op))"
    )?;
    for subsystem in &struct_tags.subsystems {
        let subsystem = subsystem.replace("_driver_api", "");

        writeln!(
            outfile,
            "#define K_SYSCALL_DRIVER_{driver_upper}(ptr, op) \
               K_SYSCALL_DRIVER_GEN(ptr, op, {driver_lower}, {driver_upper})",
            driver_lower = subsystem.to_lowercase(),
            driver_upper = subsystem.to_uppercase(),
        )?;
    }

    writeln!(outfile, "#endif /* DRIVER_VALIDATION_GEN_H */")?;

    Ok(())
}

pub fn write_kobj_types_output(mut outfile: impl Write, struct_tags: &StructTags) -> Result<()> {
    writeln!(outfile, "/* Core kernel objects */")?;
    for (kobj, obj_info) in &KOBJECTS {
        if *kobj == "device" {
            continue;
        }

        if let Some(kconfig) = obj_info.kconfig {
            writeln!(outfile, "#ifdef {kconfig}")?;
        }

        writeln!(outfile, "{},", kobject_to_enum(kobj))?;

        if obj_info.kconfig.is_some() {
            writeln!(outfile, "#endif")?;
        }
    }

    writeln!(outfile, "/* Driver subsystems */")?;
    for subsystem in &struct_tags.subsystems {
        let subsystem = subsystem.replace("_driver_api", "").to_uppercase();
        writeln!(outfile, "K_OBJ_DRIVER_{subsystem},")?;
    }

    Ok(())
}

pub fn write_kobj_otype_output(mut outfile: impl Write, struct_tags: &StructTags) -> Result<()> {
    writeln!(outfile, "/* Core kernel objects */")?;
    for (kobj, obj_info) in &KOBJECTS {
        if *kobj == "device" {
            continue;
        }

        if let Some(kconfig) = obj_info.kconfig {
            writeln!(outfile, "#ifdef {kconfig}")?;
        }

        writeln!(
            outfile,
            r#"case {}: ret = "{kobj}"; break;"#,
            kobject_to_enum(kobj)
        )?;
        if obj_info.kconfig.is_some() {
            writeln!(outfile, "#endif")?;
        }
    }

    writeln!(outfile, "/* Driver subsystems */")?;
    for subsystem in &struct_tags.subsystems {
        let subsystem = subsystem.replace("_driver_api", "");
        writeln!(
            outfile,
            r#"case K_OBJ_DRIVER_{}: ret = "{subsystem} driver"; break;"#,
            subsystem.to_uppercase()
        )?;
    }

    Ok(())
}

pub fn write_kobj_size_output(mut outfile: impl Write) -> Result<()> {
    writeln!(outfile, "/* Non device/stack objects */")?;
    for (kobj, obj_info) in &KOBJECTS {
        if !obj_info.dyn_allocatable {
            continue;
        }

        if let Some(kconfig) = obj_info.kconfig {
            writeln!(outfile, "#ifdef {kconfig}")?;
        }

        writeln!(
            outfile,
            r#"case {}: ret = sizeof(struct {kobj}); break;"#,
            kobject_to_enum(kobj)
        )?;
        if obj_info.kconfig.is_some() {
            writeln!(outfile, "#endif")?;
        }
    }

    Ok(())
}
