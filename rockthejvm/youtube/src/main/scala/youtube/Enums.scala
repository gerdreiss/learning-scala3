object Enums:
  enum Permissions:
    case READ, WRITE, EXEC, NONE

  val read = Permissions.READ

  enum PermissionsWithBits(val bits: Int):
    case READ extends PermissionsWithBits(4)  // 100
    case WRITE extends PermissionsWithBits(2) // 010
    case EXEC extends PermissionsWithBits(1)  // 001
    case NONE extends PermissionsWithBits(0)  // 000

    def toHex: String | Null = Integer.toHexString(bits)

  object PermissionsWithBits:
    def fromBits(bits: 0 | 1 | 2 | 4): PermissionsWithBits =
      bits match
        case 0 => PermissionsWithBits.NONE
        case 1 => PermissionsWithBits.EXEC
        case 2 => PermissionsWithBits.WRITE
        case 4 => PermissionsWithBits.READ

  val read2: PermissionsWithBits = PermissionsWithBits.READ
  val bitstring                  = read2.toHex

  // standard API

  val first          = Permissions.READ.ordinal
  val allPermissions = Permissions.values
  val readPermission = Permissions.valueOf("READ") // Permissions.READ
